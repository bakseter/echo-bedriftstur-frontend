import { Elm } from './Main.elm';

Elm.Main.init ({
    node: document.getElementById('elm'),
    flags: { apiKey: 'text', width: window.innerWidth, height: window.innerHeight }
});
