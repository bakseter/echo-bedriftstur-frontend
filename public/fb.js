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
    const user = firebase.auth().currentUser;

    if (!user) {
        if (debug)
            console.log("user not signed in");
        app.ports.userNotSignedIn.send(true);
    }

    db.collection("users").doc(user.uid).get()
        .then(doc => {
            if (doc.exists) {
                app.ports.getUserInfoSucceeded.send(doc.data());
            }
            else {
                if (debug)
                    console.log("user has no info, creating info");
                
                db.collection("users").doc(user.uid).set({
                    email: user.email,
                    firstName: "init",
                    lastName: "init",
                    degree: "init"
                })
                .then(docRef => {
                    const emailOnly = {
                        email: user.email
                    };
                    app.ports.getUserInfoSucceeded.send(emailOnly);
                })
                .catch(error => {
                    // TODO: handle error properly in Elm
                    if (debug)
                        console.log(error);
                    app.ports.getUserInfoError.send(error.code);
                });
            }
        })
        .catch(error => {
            // TODO: handle error properly in Elm
            if (debug)
                console.log(error);
            app.ports.getUserInfoError.send(error.code);
        });
});

app.ports.updateUserInfo.subscribe(data => {
    const user = firebase.auth().currentUser;
    
    if (user && user.emailVerified) {
        db.collection("users").doc(user.uid).set({
            email: user.email,
            firstName: data.firstName,
            lastName: data.lastName,
            degree: data.degree
        })
        .then(docRef => {
            app.ports.updateUserInfoSucceeded.send(true);
        })
        .catch(error => {
            // TODO: test that error is handled properly in Elm
            if (debug)
                console.log(error.code);
            app.ports.updateUserInfoError.send(error.code);
        });
    }
});

app.ports.attemptSignOut.subscribe(data => {
    if (data.requestedLogOut) {
        firebase.auth().signOut()
            .then(() => {
                app.ports.signOutSucceeded.send(true);
            })
            .catch(error => {
                // TODO: test that error is handled properly in Elm
                if (debug)
                    console.log(error.code);
                app.ports.signOutError.send(error);
            });
    }
});
