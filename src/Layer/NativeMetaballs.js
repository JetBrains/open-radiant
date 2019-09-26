function m(target, width, height, model, colors_) {

    if (!model) return;

    if (!model.groups.length) return;

    //const pixelRatio = window.devicePixelRatio || 1;

    console.log(model);

    const blur = model.effects.blur;
    const fat = model.effects.fat;
    const ring = model.effects.ring;

    let colors = colors_ || ['#341f49', '#f38038', '#ed3d7d'];

    let canvas;
    let gl;
    let displayWidth;
    let displayHeight;
    // additional to the one in the Elm model
    // since resize is not going through Elm
    let scale = width / 2000;
    let createdMetaballs = [];
    let isStopped = false;

    //const defaults = {
      // speedRange: {min: 0.2, max: 2.0},
      // multArc: {x: {min: -.25, max: .75}, y: {min: -.25, max: .25}},
      // originOffset: {x: 0.6, y: 0.5},
      //originOffset: {x: 0, y: 0},
      //scale: 1 //width / 1500
      // colorI: colors[1],
      // colorII: colors[2],
      // colorIII: colors[0]
    //}

    /* function calculateScale(dWidth, dHeight) {
      return dWidth / 1500; // (window.devicePixelRatio || 1)*
    } */

    initialize(target, width, height);

    function initialize(target, width, height) {

      canvas = target;
      canvas.width = width;
      canvas.height = height;
      canvas.className = 'layer';

      const glConfig = {
        premultipliedAlpha: true,
        antialias: true,
        depth: true,
        alpha: true
      };

      gl = canvas.getContext('webgl', glConfig) || canvas.getContext('experimental-webgl', glConfig);
      gl.getExtension('OES_standard_derivatives');

      if (!gl) {
        console.error('cannot find gl', gl);
        return;
      }

      displayWidth = Math.floor(gl.canvas.clientWidth);
      displayHeight = Math.floor(gl.canvas.clientHeight);
      //scale = calculateScale(displayWidth, displayHeight);

      const groups = model.groups.map(
        group => ({
          metaballs: group.balls.map(
            ball => ({
              center: {
                x: ball.x,
                y: ball.y
              },
              offset: group.origin,
              radius: ball.r,
              speed: ball.speed / 300,
              t: ball.phase,
              arcMult: { x: ball.ax / 15, y: ball.ay / 15 }
            })
          ),
          texture: generateGradientTexture(
            group.gradient.stops.map( stop => ({ color: stop.color, stop: stop.pos })),
            group.gradient.orientation === "vertical",
            false
          )
        })
      );

      groups.map(function (group) {
        createdMetaballs.push(new Metaballs(gl, group, scale));
      });

      target.addEventListener('mousemove', onMouseMove);

      resizeGL(gl);

      requestAnimationFrame(step);
    }

    function generateGradientTexture(colors, vertical, debug) {

      colors = colors || [{color: '#000000', stop: 0.0}, {color: '#FFF000', stop: .5}, {color: '#642054', stop: 1.0}];
      vertical = vertical !== undefined ? vertical : false;

      const size = 512;

      const textureCanvas = document.createElement('canvas');
      textureCanvas.width = size;
      textureCanvas.height = size;

      const context = textureCanvas.getContext('2d');

      context.rect(0, 0, size, size);

      const grd = vertical ? context.createLinearGradient(0, size, 0, 0) : context.createLinearGradient(0, 0, size, 0);
      for (let i = 0; i < colors.length; i++) {
        grd.addColorStop(colors[i].stop, colors[i].color);
      }
      context.fillStyle = grd;
      context.fillRect(0, 0, size, size);

      return textureCanvas;
    }

    function randomFromTo(range) {
      return Math.random() * (range.max - range.min) + range.min;
    }

    function randomXY(ranges) {
      return {x: randomFromTo(ranges.x), y: randomFromTo(ranges.y)}
    }

    function resize(size) {
      canvas.width = size[0];
      canvas.height = size[1];


      resizeGL(gl);
      gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);

    }

    function onMouseMove(event) {
      createdMetaballs.forEach(function (metaball) {
        metaball.handleMouseMove(event.clientX, event.clientY);
      });

    }

    function resizeGL(gl) {

      displayWidth = Math.floor(gl.canvas.clientWidth);
      displayHeight = Math.floor(gl.canvas.clientHeight);
      //scale = calculateScale(displayWidth, displayHeight);

      gl.viewport(0, 0, displayWidth, displayHeight);

      createdMetaballs.forEach(function (metaball) {
        metaball.handleResize(displayWidth, displayHeight);
      });
    }

    function updateEffects(effects) {
      createdMetaballs.forEach(function (metaball) {
        metaball.updateEffects(effects);
      });
    }

    const stop = function() { isStopped = true };

    function step() {
      createdMetaballs.forEach(function (metaball) {
        metaball.updateMetaballs();
      });
      if (!isStopped) requestAnimationFrame(step);
    };


    function Metaballs(gl, config, scale) {
      let program;
      let metaballsObjects = [];
      let metaballsObjectsHandle;
      let resolutionUniform;
      let time = 0.0;
      let colorTexture;
      let animationProperties = {
        radiusMultiplier: 1.0,
        positionMultiplier: 1.0
      };
      let mousePosition = { x: 0, y: 0 };

      function initializeShader() {

        const vertexShaderSource = compileShader(vertexShader, gl.VERTEX_SHADER);
        const fragmentShaderSource = compileShader(fragmentShader, gl.FRAGMENT_SHADER);

        program = gl.createProgram();
        gl.attachShader(program, vertexShaderSource);
        gl.attachShader(program, fragmentShaderSource);
        gl.linkProgram(program);
        gl.useProgram(program);

        const vertexData = new Float32Array([
          -1.0, 1.0,
          -1.0, -1.0,
          1.0, 1.0,
          1.0, -1.0,
        ]);
        const vertexDataBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, vertexDataBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, vertexData, gl.STATIC_DRAW);

        var positionHandle = getAttribLocation(program, 'position');

        gl.enableVertexAttribArray(positionHandle);
        gl.vertexAttribPointer(positionHandle,
          2,
          gl.FLOAT,
          gl.FALSE,
          2 * 4,
          0
        );

        metaballsObjectsHandle = getUniformLocation(program, 'metaballs');

        colorTexture = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, colorTexture);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, config.texture);
        gl.generateMipmap(gl.TEXTURE_2D);
        gl.bindTexture(gl.TEXTURE_2D, null);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, colorTexture);
        gl.uniform1i(gl.getUniformLocation(program, 'uColorSampler'), 0);

        resolutionUniform = getUniformLocation(program, 'uResolution');
        gl.uniform2f(resolutionUniform, gl.canvas.width, gl.canvas.height);

        gl.uniform1f(gl.getUniformLocation(program, 'uBlur'), blur);
        gl.uniform1f(gl.getUniformLocation(program, 'uFat'), fat);
        gl.uniform1f(gl.getUniformLocation(program, 'uRing'), ring);
      }


      function compileShader(shaderSource, shaderType) {
        var shader = gl.createShader(shaderType);
        gl.shaderSource(shader, shaderSource);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
          throw "Shader compile failed with: " + gl.getShaderInfoLog(shader);
        }
        return shader;
      }


      function getAttribLocation(program, name) {
        var attributeLocation = gl.getAttribLocation(program, name);
        if (attributeLocation === -1) {
          throw 'Can not find attribute ' + name + '.';
        }
        return attributeLocation;
      }


      function getUniformLocation(program, name) {
        var uniformLocation = gl.getUniformLocation(program, name);
        if (uniformLocation === -1) {
          throw 'Can not find uniform ' + name + '.';
        }
        return uniformLocation;
      }

      var dataToSendToGPU;

      function setupAttributes() {
        time += 0.01;
        var count = config.metaballs.length;

        for (var i = 0; i < count; i++) {
          var metaball = config.metaballs[i];
          metaball.x = (displayWidth * metaball.offset.x) + (metaball.center.x * scale);
          metaball.y = (displayHeight * metaball.offset.y) + (metaball.center.y * scale);
          metaball.targRadius = metaball.radius * scale + ((Math.cos((metaball.t + time) * metaball.speed) * 5) + (Math.sin((metaball.t + time) * metaball.speed) * 5));// * animationProperties.positionMultiplier;
        }

        dataToSendToGPU = new Float32Array(3 * count);
        for (var i = 0; i < count; i++) {
          var baseIndex = 3 * i;
          var mb = metaballsObjects[i];

          dataToSendToGPU[baseIndex + 0] = mb.x;
          dataToSendToGPU[baseIndex + 1] = mb.y;
          dataToSendToGPU[baseIndex + 2] = mb.radius;
        }
        gl.uniform3fv(metaballsObjectsHandle, dataToSendToGPU);
      }


      function setupMetaballs() {
        metaballsObjects = config.metaballs;
        var metaball;
        for (var i = 0, total = metaballsObjects.length; i < total; i++) {
          metaball = metaballsObjects[i];
          metaball.ox = metaball.x = (displayWidth * metaball.offset.x) + metaball.center.x * scale;
          metaball.oy = metaball.y = (displayHeight * metaball.offset.y) + metaball.center.y * scale;
        }
      }

      this.handleResize = function (width, height) {
        gl.useProgram(program);
        gl.uniform2f(resolutionUniform, width, height);
      }

      this.updateEffects = function (effects) {
        gl.useProgram(program);
        gl.uniform1f(gl.getUniformLocation(program, 'uBlur'), effects.blur);
        gl.uniform1f(gl.getUniformLocation(program, 'uFat'), effects.fat);
        gl.uniform1f(gl.getUniformLocation(program, 'uRing'), effects.ring);
      }

      this.handleMouseMove = function (x, y) {
        mousePosition.x = x - (window.innerWidth - width) / 2;
        mousePosition.y = window.innerHeight - y;
      }


      this.updateMetaballs = function () {

        time += 0.01;

        var count = config.metaballs.length;

        var radius = 30;
        var targX, targY, t, d, mb;
        for (var i = 0; i < count; i++) {
          mb = metaballsObjects[i];
          targX = (mb.offset.x * displayWidth) + (mb.center.x * scale + (Math.sin((mb.t + time) * mb.speed) * radius * mb.arcMult.x) + (Math.sin((mb.t + time) * mb.speed) * radius * mb.arcMult.x)) * animationProperties.positionMultiplier;
          targY = (mb.offset.y * displayHeight) + (mb.center.y * scale + (Math.cos((mb.t + time) * mb.speed) * radius * mb.arcMult.y) + (Math.cos((mb.t + time) * mb.speed) * radius * mb.arcMult.y)) * animationProperties.positionMultiplier;

          t = Math.atan2(mb.x - mousePosition.x, mb.y - mousePosition.y);
          d = 500 / Math.sqrt(Math.pow(mousePosition.x - mb.x, 2) + Math.pow(mousePosition.y - mb.y, 2));
          mb.x += d * Math.sin(t) + (targX - mb.x) * 0.1;
          mb.y += d * Math.cos(t) + (targY - mb.y) * 0.1;
        }


        for (var i = 0; i < count; i++) {
          var baseIndex = 3 * i;
          var mb = metaballsObjects[i];
          dataToSendToGPU[baseIndex + 0] = mb.x;
          dataToSendToGPU[baseIndex + 1] = mb.y;
          dataToSendToGPU[baseIndex + 2] = (mb.radius * scale * animationProperties.radiusMultiplier);
        }

        gl.useProgram(program);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, colorTexture);
        gl.enable(gl.BLEND);
        gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
        gl.uniform3fv(metaballsObjectsHandle, dataToSendToGPU);
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

      };


      const vertexShader = `
          attribute vec2 position;
           void main() {
          gl_Position = vec4(position, 0.0, 1.0);
          }
  `;

      const fragmentShader = `
            #ifdef GL_OES_standard_derivatives
              #extension GL_OES_standard_derivatives : enable
            #endif
            precision highp float;
            const int NUM_METABALLS = 15;

            uniform vec3 metaballs[15];
            uniform vec2 uResolution;
            uniform sampler2D uColorSampler;
            uniform float uBlur;
            uniform float uFat;
            uniform float uRing;

            float v = 0.0;


            float noise(vec2 seed, float time) {
                float x = (seed.x / 3.14159 + 4.0) * (seed.y / 13.0 + 4.0) * ((fract(time) + 1.0) * 10.0);
                return mod((mod(x, 13.0) + 1.0) * (mod(x, 123.0) + 1.0), 0.01) - 0.005;
            }

            float brightness(vec3 color) {
                return (0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b);
            }

            vec3 rgb2hsv(vec3 c){
              vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
              vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
              vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

              float d = q.x - min(q.w, q.y);
              float e = 1.0e-10;
              return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
              }

              vec3 hsv2rgb(vec3 c){
                vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
                vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
                return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
            }

            float blur = 1.0 - uBlur;
            float fat = 4.0 * uFat + 0.2;
            float ring = 1.0 - uRing;

            void main(){
               vec2 cpos = gl_FragCoord.xy;
               for (int i = 0; i < NUM_METABALLS; i++) {
                    vec3 mb = metaballs[i];
                    vec2 deltaPos = mb.xy - cpos;
                    float r = mb.z;
                    v += r*r/dot( deltaPos, deltaPos );
                }

                float delta = 0.0, alpha = 1.0;
               // vec4 color = vec4(1.0);
                vec4 color = texture2D(uColorSampler, gl_FragCoord.xy / uResolution.xy);

                #ifdef GL_OES_standard_derivatives
                    delta = fwidth(v);
                    if ( v > delta && v < delta + 1.0 + 2.0 * ring ) {
                      alpha = smoothstep( blur - delta, 1.0, v * fat );
                    }

                    else {
                      alpha = ring + smoothstep(1.0, blur - delta,  1.0 );
                      }


                    // if ( v < delta + 2.1){
                    //   alpha =  smoothstep( delta, 1.0, v * fat );
                    // }
                    // //  else {
                    // //    alpha = 0.0;
                    // //    }

                #else

                  if (v > 1.0) {
                      float l = length(color);

                      if (l > 1.05) {

                        color *=  0.7;
                      } else {

                        color *= 0.5;
                      }

                  } else {

                    discard;
                  }

                #endif

                 vec2 st = gl_FragCoord.xy / uResolution.xy;

                 //ambient light
                 color.a *=  smoothstep(-0.1, 0.5, distance(st, vec2(0.5)));
                 color.rgb *= vec3(1.0, 0.5, 0.7);
                 //noise
                 color.rgb = mix(color.rgb, vec3(noise(st * 1000.0, 1.0) * 100.0), 0.03 / pow(brightness(color.rgb), 0.3));

                 gl_FragColor = color * alpha * 0.8;


              }

  `;

      initializeShader();
      setupMetaballs();
      setupAttributes();

    }

    return {
      size : [ width, height ],
      palette: colors,
      model,
      stop,
      resize,
      update : m,
      updateEffects
    };

  };

  module.exports = m;
