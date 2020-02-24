const debug = true;

var app = Elm.Main.init ({
    node: document.getElementById("elm"),
    flags: window.location.pathname
});

var firebaseConfig = {
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

app.ports.sendSignInLink.subscribe(function(data) {
    const actionCodeSettings = {
        // TODO: change this in production
        url : "https://echobedriftstur-userauth.firebaseapp.com/verified",
        handleCodeInApp : true
    };
    firebase.auth().sendSignInLinkToEmail(data.email, actionCodeSettings).then(function() {
        window.localStorage.setItem("emailForSignIn", data.email);
        app.ports.sendSignInLinkSucceeded.send(true);
    })
    .catch(function(error) {
        // Error is handled in Elm
        app.ports.sendSignInLinkError.send(error.code);
    });
});

if (firebase.auth().isSignInWithEmailLink(window.location.href)) {
    var email = window.localStorage.getItem("emailForSignIn");

    if (!email) {
        email = window.prompt("Vennligst skriv inn din studentmail for verifikasjon");
    }

    firebase.auth().signInWithEmailLink(email, window.location.href).then(function(result) {
        window.localStorage.removeItem("emailForSignIn");
        app.ports.signInSucceeded.send(result);
    })
    .catch(function(error) {
        // TODO: test that error is handled properly in Elm
        if (debug) {
            console.log(error.code);
            console.log(error);
        }
        app.ports.signInWithLinkError.send(error.code);
    });
}

app.ports.getUserInfo.subscribe(function(data) {
    const user = firebase.auth().currentUser;

    if (!user) {
        if (debug)
            console.log("Du er ikke logget inn");
        app.ports.userNotSignedIn.send(true);
    }

    db.collection("users").doc(user.uid).get().then(function(doc) {
        if (doc.exists) {
            app.ports.getUserInfoSucceeded.send(doc.data());
        }
        else {
            console.log("user has no info, creating info");
            db.collection("users").doc(user.uid).set({
                email: user.email,
                firstName: "",
                lastName: "",
                degree: ""
           }).then(function(docRef) {
                const emailOnly = {
                    email: user.email
                };
                app.ports.getUserInfoSucceeded.send(emailOnly);
           }).catch(function(error) {
                // TODO: handle error properly in Elm
                if (debug)
                    console.log(error);
                app.ports.getUserInfoError.send(error.code);
           });
        }
    }).catch(function(error) {
        // TODO: handle error properly in Elm
        if (debug)
            console.log(error);
        app.ports.getUserInfoError.send(error.code);
    });
});

app.ports.updateUserInfo.subscribe(function(data) {
    const user = firebase.auth().currentUser;
    
    if (user && user.emailVerified) {
        db.collection("users").doc(user.uid).set({
            email: user.email,
            firstName: data.firstName,
            lastName: data.lastName,
            degree: data.degree
        })
        .then(function(docRef) {
            app.ports.updateUserInfoSucceeded.send(true);
        })
        .catch(function(error) {
            // TODO: test that error is handled properly in Elm
            if (debug)
                console.log(error.code);
            app.ports.updateUserInfoError.send(error.code);
        });
    }
});

app.ports.attemptSignOut.subscribe(function(data) {
    if (data.requestedLogOut) {
        firebase.auth().signOut().then(function() {
            app.ports.signOutSucceeded.send(true);
        })
        .catch(function(error) {
            // TODO: test that error is handled properly in Elm
            if (debug)
                console.log(error.code);
            app.ports.signOutError.send(error);
        });
    }
});
