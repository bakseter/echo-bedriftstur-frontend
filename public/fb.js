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

firebase.auth().setPersistence(firebase.auth.Auth.Persistence.SESSION)
.then(() => {
    if (debug) {
        console.log("persistence set to SESSION");
    }
})
.catch(error => {
    if (debug) {
        console.log("error setting persistence to SESSION");
    }
});

const db = firebase.firestore();

const app = Elm.Main.init ({
    node: document.getElementById("elm")
});

const getUserInfo = (col, doc, email) => {
    db.collection(col).doc(doc).get()
    .then(newDoc => {
        if (newDoc.exists) {
            app.ports.getUserInfoSucceeded.send(newDoc.data());
            if (debug) {
                console.log("Got user info: ", newDoc.data());
            }
        }
        else {
            const emailOnly = {
                email: email,
                firstName: null,
                lastName: null,
                degree: null
            };
            updateUserInfo(col, doc, emailOnly);
            app.ports.getUserInfoSucceeded.send(emailOnly);
            if (debug) {
                console.log("Create user info: ", emailOnly);
            }
        }
    })
    .catch(error => {
        if (debug) {
            console.log(error.code);
            console.log(error);
        }
    });
};

const updateUserInfo = (col, doc, data) => {
    db.collection(col).doc(doc).set(data)
    .then(docRef => {
        if (data.firstName !== null && data.lastName !== null && data.degree !== null) {
            app.ports.updateUserInfoSucceeded.send(true);
        }
    })
    .catch(error => {
        if (debug) {
            console.log(error.code);
            console.log(error);
        }
        app.ports.updateUserInfoError.send(error.code);
    });
};

firebase.auth().onAuthStateChanged(user => {
    var signedIn = "";
    if (user) {
        signedIn = "signed in";
    }
    else {
        signedIn = "signed out";
    }
    if (debug) {
        console.log("user status changed to ", signedIn);
    }
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
        if (debug) {
            console.log(result);
        }
        app.ports.signInSucceeded.send(result.user);
    })
    .catch(error => {
        // TODO: test that error is handled properly in Elm
        if (debug) {
            console.log(error.code);
            console.log(error);
        }
        app.ports.signInError.send(error.code);
    });
}

app.ports.getUserInfo.subscribe(data => {
    getUserInfo(data.collection, data.uid, data.email);
});

app.ports.updateUserInfo.subscribe(data => {
    const content = { email: data.email
                    , firstName: data.firstName
                    , lastName: data.lastName
                    , degree: data.degree
                    };
    updateUserInfo(data.collection, data.uid, content);
});

app.ports.attemptSignOut.subscribe(data => {
    if (data.requestedSignOut) {
        firebase.auth().signOut()
            .then(() => {
                app.ports.signOutSucceeded.send(true);
            })
            .catch(error => {
                // TODO: test that error is handled properly in Elm
                if (debug) {
                    console.log(error.code);
                    console.log(error);
                }
                app.ports.signOutError.send(error);
            });
    }
    else {
        if (debug) {
            console.log("user didn't request sign out???");
        }
    }
});
