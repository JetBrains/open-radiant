FROM node:8

RUN mkdir /app

WORKDIR /app

COPY . /app

RUN mkdir -p ./storage

RUN rm -Rf ./node_modules

RUN npm install

RUN npm install elm

# NODE_ENV=production / development
RUN npm run build && npm run build:player

FROM nginx:1.15

COPY --from=0 /app /usr/share/nginx/html
