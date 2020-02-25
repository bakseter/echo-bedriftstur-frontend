const debug = true;

const firebaseConfig = {
    apiKey: "AIzaSyD-mFQI_DHw8mjhomuAcPUHAwOneYhxEK8",
    authDomain: "echo-bedriftstur-81a2e.firebaseapp.com",
    databaseURL: "https://echo-bedriftstur-81a2e.firebaseio.com",
    projectId: "echo-bedriftstur-81a2e",
    storageBucket: "echo-bedriftstur-81a2e.appspot.com",
    messagingSenderId: "929375228711",
    appId: "1:929375228711:web:1cfbcadd8e54e97cd14f02",
    measurementId: "G-F0QKBQFGLD"
};
firebase.initializeApp(firebaseConfig);

const db = firebase.firestore();

const app = Elm.Main.init ({
    node: document.getElementById("elm"),
    flags: window.location.pathname
});

firebase.auth().onAuthStateChanged(user => {
    app.ports.userStatusChanged.send(user);
});

app.ports.sendSignInLink.subscribe(data => {
    const actionCodeSettings = {
        // TODO: change this in production
        url : "https://echobedriftstur-userauth.firebaseapp.com/verified",
        handleCodeInApp : true
    };
    firebase.auth().sendSignInLinkToEmail(data.email, actionCodeSettings)
    .then(() => {
        window.localStorage.setItem("emailForSignIn", data.email);
        app.ports.sendSignInLinkSucceeded.send(true);
    })
    .catch(error => {
        // Error is handled in Elm
        app.ports.sendSignInLinkError.send(error.code);
    });
});

if (firebase.auth().isSignInWithEmailLink(window.location.href)) {
    var email = window.localStorage.getItem("emailForSignIn");

    if (!email) {
        email = window.prompt("Vennligst skriv inn din studentmail for verifikasjon");
    }

    firebase.auth().signInWithEmailLink(email, window.location.href)
        .then(result => {
            window.localStorage.removeItem("emailForSignIn");
            app.ports.signInSucceeded.send(result);
        })
        .catch(error => {
            // TODO: test that error is handled properly in Elm
            if (debug) {
                console.log(error.code);
                console.log(error);
            }
            app.ports.signInWithLinkError.send(error.code);
        });
}

app.ports.getUserInfo.subscribe(data => {
    getUserInfo(data.collection, data.uid, data.content);
});

app.ports.updateUserInfo.subscribe(data => {
    updateUserInfo(data.collection, data.uid, data.content);
});

app.ports.attemptSignOut.subscribe(data => {
    if (data.requestedLogOut) {
        firebase.auth().signOut()
            .then(() => {
                app.ports.signOutSucceeded.send(true);
            })
            .catch(error => {
                // TODO: test that error is handled properly in Elm
                if (debug) {
                    console.log(error);
                    console.log(error.code);
                }
                app.ports.signOutError.send(error);
            });
    }
});

const getUserInfo = (col, doc, data) => {
    db.collection(col).doc(doc).get()
    .then(newDoc => {
        if (newDoc.exists) {
            app.ports.getUserInfoSucceeded.send(newDoc.data());
        }
        else {
            const emailOnly = {
                email: data.email,
                firstName: "init",
                lastName: "init",
                degree: "init"
            };
            updateUserInfo(col, doc, emailOnly);
            app.ports.getUserInfoSucceeded(true);
        }
    })
    .catch(error => {
        if (debug) {
            console.log(error);
            console.log(error.code);
        }
    });
};

const updateUserInfo = (col, doc, data) => {
    db.collection(col).doc(doc).set(data)
    .then(docRef => {
        app.ports.updateUserInfoSucceeded.send(true);
    })
    .catch(error => {
        if (debug) {
            console.log(error);
            console.log(error.code);
        }
        app.ports.updateUserInfoError.send(error.code);
    });
};
