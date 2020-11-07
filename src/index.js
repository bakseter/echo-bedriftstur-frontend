import * as firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/firestore';

import { Elm } from './Main.elm';

const firebaseConfig =
  { apiKey: process.env.ELM_APP_API_KEY
  , authDomain: process.env.ELM_APP_AUTH_DOMAIN
  , databaseURL: process.env.ELM_APP_DATABASE_URL
  , projectId: process.env.ELM_APP_PROJECT_ID
  , storageBucket: process.env.ELM_APP_STORAGE_BUCKET
  , messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID
  , appId: process.env.ELM_APP_APP_ID
  , measurementId: process.env.ELM_APP_MEASUREMENT_ID
};
firebase.initializeApp(firebaseConfig);

const app = Elm.Main.init ({
    node: document.getElementById('elm'),
    flags: firebaseConfig.apiKey
});

firebase.auth().onAuthStateChanged(user => {
    app.ports.userStatusChanged.send(user);
});

app.ports.sendSignInLink.subscribe(data => {
    console.log(data);
    console.log(data.actionCodeSettings);

    firebase.auth().sendSignInLinkToEmail(data.email, data.actionCodeSettings)
    .then(() => {
        window.localStorage.setItem("emailForSignIn", data.email);
        app.ports.sendSignInLinkSucceeded.send(true);
    })
    .catch(error => {
        app.ports.sendSignInLinkError.send(error.code);
        console.log(error);
        console.log(error.code);
    });
});

if (firebase.auth().isSignInWithEmailLink(window.location.href)) {
    var email = window.localStorage.getItem("emailForSignIn");

    if (!email) {
        email = window.prompt("Vennligst skriv inn email.");
    }

    firebase.auth().signInWithEmailLink(email, window.location.href)
    .then(result => {
        window.localStorage.removeItem("emailForSignIn");
        app.ports.signInSucceeded.send(result.user);
        console.log(result);
        console.log(result.user);
    })
    .catch(error => {
        app.ports.signInError.send(error.code);
        console.log(error);
        console.log(error.code);
    });
}
