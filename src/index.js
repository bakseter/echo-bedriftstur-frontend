import { Elm } from './Main';

Elm.Main.init ({
    node: document.getElementById('elm'),
    flags: { apiKey: 'text', width: window.innerWidth, height: window.innerHeight }
});
