FROM node:alpine

WORKDIR /app
COPY . /app

RUN yarn
RUN yarn elm make src/Main.elm

EXPOSE 3000
