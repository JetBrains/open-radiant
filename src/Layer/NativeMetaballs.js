function m(target, width, height, model, colors_) {

    if (!model) return;
    
    console.log(model, colors_, width, height);

    let colors = colors_ || ['#341f49', '#f38038', '#ed3d7d'];

    let canvas;
    let gl;
    let displayWidth;
    let displayHeight;
    let createdMetaballs = [];
    let isStopped = false;

    const defaults = {
      speedRange: {min: 0.2, max: 2.0},
      multArc: {x: {min: -.25, max: .75}, y: {min: -.25, max: .25}},
      originOffset: {x: 0.6, y: 0.5},
      scale: width / 1500,
      colorI: colors[1],
      colorII: colors[2],
      colorIII: colors[0]
    }

    const stop = function() { isStopped = true };

    initialize(target, width, height);

    function initialize(target, width, height) {

      canvas = target;
      canvas.width = width;
      canvas.height = height;

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


      const groups = [
        {
          metaballs: [
            {
              center: {x: -50, y: 220},
              radius: 50,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -20, y: 85},
              radius: 90,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -30, y: 50},
              radius: 60,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 170, y: 170},
              radius: 100,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },

            {
              center: {x: 370, y: 30},
              radius: 40,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 450, y: 150},
              radius: 50,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 270, y: -240},
              radius: 70,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 370, y: -130},
              radius: 60,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 170, y: -70},
              radius: 70,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },

          ],
          texture: generateGradientTexture([
              {color: defaults.colorIII, stop: 0.2},
              {color: defaults.colorI, stop: 0.3},
              {color: defaults.colorII, stop: 0.5},
              {color: defaults.colorIII, stop: 0.8}],
            true, false)
        },
        {
          metaballs: [
            {
              center: {x: 150, y: 350},
              radius: 30,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },

            {
              center: {x: 250, y: 250},
              radius: 70,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },

            {
              center: {x: 380, y: 280},
              radius: 30,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },

            {
              center: {x: 200, y: 100},
              radius: 25,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },


          ],
          texture: generateGradientTexture([
              {color: defaults.colorII, stop: 0.2},
              {color: defaults.colorIII, stop: 0.3},
              {color: defaults.colorII, stop: 0.5},
              {color: defaults.colorIII, stop: 0.7}],
            true, false)
        },
        {
          metaballs: [
            {
              center: {x: 410, y: -80},
              radius: 28,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 340, y: -100},
              radius: 70,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 200, y: -150},
              radius: 40,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 250, y: -200},
              radius: 36,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
          ],
          texture: generateGradientTexture([
              {color: defaults.colorI, stop: 0.50},
              {color: defaults.colorII, stop: 0.6},
              {color: defaults.colorIII, stop: 0.70}],
            true, false)
        },
        {
          metaballs: [
            {
              center: {x: -410, y: -270},
              radius: 48,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -490, y: -230},
              radius: 34,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -470, y: -320},
              radius: 40,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -700, y: 250},
              radius: 30,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -740, y: 310},
              radius: 20,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
          ],
          texture: generateGradientTexture(
            [
              {color: defaults.colorIII, stop: 0.3},
              {color: defaults.colorII, stop: 0.4},
              {color: defaults.colorII, stop: 0.68},
              {color: defaults.colorIII, stop: 0.77},
              {color: defaults.colorIII, stop: 0.83}
            ], true, false)
        },

        {
          metaballs: [
            {
              center: {x: -830, y: 40},
              radius: 30,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -700, y: 90},
              radius: 60,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -540, y: 270},
              radius: 50,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -490, y: 150},
              radius: 90,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -300, y: 240},
              radius: 40,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -200, y: 120},
              radius: 35,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -350, y: 50},
              radius: 70,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -490, y: -40},
              radius: 60,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -270, y: -70},
              radius: 50,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
          ],
          texture: generateGradientTexture(
            [
              {color: defaults.colorIII, stop: 0.3},
              {color: defaults.colorII, stop: 0.5},
              {color: defaults.colorI, stop: 0.6}
            ], true, false)
        },
        {
          metaballs: [
            {
              // center: {x: randomFromTo({min: 10, max: 26}), y: 155},
              center: {x: 26, y: 55},
              radius: 120,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -110, y: -90},
              radius: 60,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: 12, y: -214},
              radius: 80,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -300, y: -80},
              radius: 120,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
            {
              center: {x: -570, y: -120},
              radius: 50,
              speed: randomFromTo(defaults.speedRange),
              t: Math.random() * 200,
              arcMult: randomXY(defaults.multArc)
            },
          ],

          texture: generateGradientTexture(
            [
              {color: defaults.colorI, stop: 0.2},
              {color: defaults.colorII, stop: 0.35},
              {color: defaults.colorIII, stop: 0.55},
              {color: defaults.colorII, stop: 0.75},
              {color: defaults.colorI, stop: 1.0}
            ], true, false)
        }


      ]

      groups.map(function (group) {
        createdMetaballs.push(new Metaballs(gl, group, defaults.scale));
      })

      target.addEventListener('mousemove', onMouseMove);

      resizeGL(gl);

      step();
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

    function onWindowResize(event) {
      canvas.width = canvas.clientWidth;
      canvas.height = canvas.clientHeight;


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

      gl.viewport(0, 0, displayWidth, displayHeight);

      createdMetaballs.forEach(function (metaball) {
        metaball.handleResize(displayWidth, displayHeight);
      });
    }

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
      let mousePosition = {x: 0, y: 0};

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
        var centerX = displayWidth * defaults.originOffset.x;
        var centerY = displayHeight * defaults.originOffset.y;


        for (var i = 0; i < count; i++) {
          var metaball = config.metaballs[i];
          metaball.x = centerX + (metaball.center.x * scale);
          metaball.y = centerY + (metaball.center.y * scale);
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
        var centerX = displayWidth * defaults.originOffset.x;
        var centerY = displayHeight * defaults.originOffset.y;
        for (var i = 0, total = metaballsObjects.length; i < total; i++) {
          metaball = metaballsObjects[i];
          metaball.ox = metaball.x = centerX + metaball.center.x * scale;
          metaball.oy = metaball.y = centerY + metaball.center.y * scale;
        }
      }

      this.handleResize = function (width, height) {
        gl.useProgram(program);
        gl.uniform2f(resolutionUniform, width, height);
      }

      this.handleMouseMove = function (x, y) {
        mousePosition.x = x - (window.innerWidth - width ) / 2;
        mousePosition.y = y;
      }


      this.updateMetaballs = function () {

        time += 0.01;

        var count = config.metaballs.length;
        var centerX = displayWidth * defaults.originOffset.x;
        var centerY = displayHeight * defaults.originOffset.y;

        var radius = 30;
        var targX, targY, t, d, mb;
        for (var i = 0; i < count; i++) {
          mb = metaballsObjects[i];
          targX = centerX + (mb.center.x * scale + (Math.sin((mb.t + time) * mb.speed) * radius * mb.arcMult.x) + (Math.sin((mb.t + time) * mb.speed) * radius * mb.arcMult.x)) * animationProperties.positionMultiplier;
          targY = centerY + (mb.center.y * scale + (Math.cos((mb.t + time) * mb.speed) * radius * mb.arcMult.y) + (Math.cos((mb.t + time) * mb.speed) * radius * mb.arcMult.y)) * animationProperties.positionMultiplier;

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

            void main(){
               vec2 cpos = gl_FragCoord.xy;
               for (int i = 0; i < NUM_METABALLS; i++) {
                    vec3 mb = metaballs[i];
                    vec2 deltaPos = mb.xy - cpos;
                    float r = mb.z;
                    v += r*r/dot( deltaPos, deltaPos );
                }

                float delta = 0.0, alpha = 1.0;
                vec4 color = texture2D(uColorSampler, gl_FragCoord.xy / uResolution.xy);

                #ifdef GL_OES_standard_derivatives
                    delta = fwidth(v);
                    if ( v > delta) {
                      alpha = smoothstep( 1.0 - delta, 1.0 + delta, v);
                    }
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

    return { width, height, stop };

  };

  module.exports = {
    start: m,
    update: m
  };
