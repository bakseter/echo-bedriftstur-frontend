import { Elm } from './Main';

Elm.Main.init ({
    node: document.getElementById('elm'),
    flags: process.env.ELM_APP_API_KEY
});
