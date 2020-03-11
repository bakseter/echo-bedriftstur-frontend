# echo bedriftstur

[![Build Status](https://travis-ci.org/bakseter/echo-bedriftstur.svg?branch=master)](https://travis-ci.org/bakseter/echo-bedriftstur)

## Info

Website of echo bedriftstur - a student trip to Oslo organized by volunteers from _echo - Fagutvalget for Informatikk_ at the University of Bergen.
The website is written in Elm and hosted using Firebase.

## Local setup

**1. Clone the git repository**
    
    git clone https://github.com/bakseter/echo-bedriftstur

**2. Install dependencies**

    sudo npm install -g firebase-tools elm

**4. Build the project**
    
    elm make src/Main.elm --output public/main.js

**3. Host the website locally**

    firebase serve
