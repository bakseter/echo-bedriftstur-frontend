import * as firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/firestore';
import 'firebase/database';
import * as assets from 'assets/*.png'

console.log(assets.default);

import { Elm } from './Main';

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

const db: firebase.database.Database = firebase.database();
const auth: firebase.auth.Auth = firebase.auth();

const app = Elm.Main.init ({
    node: document.getElementById("elm"),
    flags: assets.default
});


auth.onAuthStateChanged(user => {
    app.ports.userStatusChanged.send(user);
});


app.ports.sendSignInLink.subscribe(data => {
    auth.sendSignInLinkToEmail(data.email, data.actionCodeSettings)
    .then(() => {
        window.localStorage.setItem("emailForSignIn", data.email);
        app.ports.sendSignInLinkSucceeded.send(true);
    })
    .catch(error => {
        app.ports.sendSignInLinkError.send(error.code);
    });
});

if (auth.isSignInWithEmailLink(window.location.href)) {
    var email = window.localStorage.getItem("emailForSignIn");

    if (!email) {
        email = window.prompt("Vennligst skriv inn mailen du logget inn med for verifikasjon.");
    }

    firebase.auth().signInWithEmailLink(email, window.location.href)
    .then(result => {
        window.localStorage.removeItem("emailForSignIn");
        app.ports.signInSucceeded.send(result.user);
    })
    .catch(error => {
        app.ports.signInError.send(error.code);
    });
}

app.ports.getUserInfo.subscribe(data => {
    const ref = db.ref('/users/' + data.uid);

    ref.on('value', (snapshot) => {
            if (snapshot.exists()) {
                app.ports.getUserInfoSucceeded.send(snapshot.val());
            }
            else {
                app.ports.getUserInfoError.send('unavailable');
            }
            console.log(snapshot);
            console.log(snapshot.val());
        });
    // getUserInfo(data.collection, data.uid, data.email);
});

app.ports.updateUserInfo.subscribe(data => {
    const content = { firstName: data.firstName
                    , lastName: data.lastName
                    , degree: data.degree
                    , terms: data.terms
                    };
    // updateUserInfo(data.collection, data.uid, content);
});

app.ports.createTicket.subscribe(data => {
    const newData =
        { timestamp: firebase.firestore.FieldValue.serverTimestamp() 
        , submittedTicket: data.submittedTicket
        };
    // createTicket(data.collection, data.uid, newData);
});

app.ports.signOut.subscribe(() => {
    firebase.auth().signOut()
        .then(() => {
            app.ports.signOutSucceeded.send(true);
        })
        .catch(error => {
            app.ports.signOutError.send(error);
        });
});
