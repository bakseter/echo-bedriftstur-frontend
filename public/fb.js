const firebaseConfig = 
    { apiKey: "AIzaSyD-mFQI_DHw8mjhomuAcPUHAwOneYhxEK8"
    , authDomain: "echo-bedriftstur-81a2e.firebaseapp.com"
    , databaseURL: "https://echo-bedriftstur-81a2e.firebaseio.com"
    , projectId: "echo-bedriftstur-81a2e"
    , storageBucket: "echo-bedriftstur-81a2e.appspot.com"
    , messagingSenderId: "929375228711"
    , appId: "1:929375228711:web:1cfbcadd8e54e97cd14f02"
    , measurementId: "G-F0QKBQFGLD"
    };
firebase.initializeApp(firebaseConfig);

const db = firebase.firestore();

const app = Elm.Main.init ({
    node: document.getElementById("elm")
});

const getUserInfo = (col, doc, data) => {
    db.collection(col).doc(doc).get()
    .then(docRef => {
        if (docRef.exists) {
            app.ports.getUserInfoSucceeded.send(docRef.data());
        }
        else {
            const emailOnly = { email: data };

            db.collection(col).doc(doc).set(emailOnly)
            .then(newDoc => {
                app.ports.getUserInfoSucceeded.send(emailOnly);
            })
            .catch(error => {
                app.ports.getUserInfoError.send(error.code);
            });
        }
    })
    .catch(error => {
        app.ports.getUserInfoError.send(error.code);
    });
};

const createTicket = (col, doc, data) => {
    db.collection(col).doc(doc).update(data)
    .then(docRef => {
        app.ports.createTicketSucceeded.send(true);
    })
    .catch(error => {
        app.ports.createTicketError.send(error.code);
    });
};

const updateUserInfo = (col, doc, data) => {
    db.collection(col).doc(doc).update(data)
    .then(docRef => {
        if (data.firstName !== null && data.lastName !== null && data.degree !== null && data.terms !== null) {
            app.ports.updateUserInfoSucceeded.send(true);
        }
    })
    .catch(error => {
        app.ports.updateUserInfoError.send(error.code);
    });
};

firebase.auth().onAuthStateChanged(user => {
    app.ports.userStatusChanged.send(user);
});

app.ports.sendSignInLink.subscribe(data => {
    const actionCodeSettings = {
        url : "https://echobedriftstur.no/verified",
        handleCodeInApp : true
    };
    firebase.auth().sendSignInLinkToEmail(data.email, actionCodeSettings)
    .then(() => {
        window.localStorage.setItem("emailForSignIn", data.email);
        app.ports.sendSignInLinkSucceeded.send(true);
    })
    .catch(error => {
        app.ports.sendSignInLinkError.send(error.code);
    });
});

if (firebase.auth().isSignInWithEmailLink(window.location.href)) {
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
    getUserInfo(data.collection, data.uid, data.email);
});

app.ports.updateUserInfo.subscribe(data => {
    const content = { firstName: data.firstName
                    , lastName: data.lastName
                    , degree: data.degree
                    , terms: data.terms
                    };
    updateUserInfo(data.collection, data.uid, content);
});

app.ports.createTicket.subscribe(data => {
    const newData =
        { timestamp: firebase.firestore.FieldValue.serverTimestamp() 
        , submittedTicket: data.submittedTicket
        };
    createTicket(data.collection, data.uid, newData);
});

app.ports.attemptSignOut.subscribe(data => {
    firebase.auth().signOut()
        .then(() => {
            app.ports.signOutSucceeded.send(true);
        })
        .catch(error => {
            app.ports.signOutError.send(error);
        });
});
