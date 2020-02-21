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

var db = firebase.firestore();

app.ports.sendSignInLink.subscribe(function(data) {
    var actionCodeSettings = {
        url : "https://echobedriftstur-userauth.firebaseapp.com/verified",
        handleCodeInApp : true
    };
    firebase.auth().sendSignInLinkToEmail(data.email, actionCodeSettings)
        .then(function() {
            window.localStorage.setItem("emailForSignIn", data.email);
        })
        .catch(function(error) {
            app.ports.sendSignInLinkError.send(error.code);
            console.log(error.code);
        });
});

if (firebase.auth().isSignInWithEmailLink(window.location.href)) {
    var email = window.localStorage.getItem("emailForSignIn");
    if (!email) {
        email = window.prompt("Vennligst skriv inn din mail for verifikasjon");
    }

    firebase.auth().signInWithEmailLink(email, window.location.href)
        .then(function(result) {
            window.localStorage.removeItem("emailForSignIn");
            app.ports.signInSucceeded.send(result);
            console.log(result);
        })
        .catch(function(error) {
            app.ports.signInWithLinkError.send(error.code);
            console.log(error.code);
        });
}

app.ports.getUserInfo.subscribe(function(data) {
    if (data.getUserInfo) {
        var user = firebase.auth().currentUser;

        if (!user) {
            console.log("Du er ikke logget inn");
            return;
        }

        db.collection("users").where("email", "==", user.email)
            .get()
            .then(function(querySnapshot) {
                if (querySnapshot.empty) {
                    console.log("user info empty at ", user.email);
                    app.ports.userInfoEmpty.send(true);
                    return;
                }
                else {
                    querySnapshot.forEach(function(doc) {
                        console.log(doc.id, " => ", doc.data());
                        app.ports.gotUserInfo.send(doc.data());
                    });
                }
            })
            .catch(function(error) {
                console.log(error.code);
                app.ports.getUserInfoError.send(error.code);
            });
    }
    else {
        console.log("getUserInfo is false?????");
    }
});

app.ports.updateUserInfo.subscribe(function(data) {
    var user = firebase.auth().currentUser;
    
    if (user && user.emailVerified) {
        if (data.userInfoEmpty) {
            db.collection("users").doc(user.uid).set({
                email: user.email,
                firstName: data.firstName,
                lastName: data.lastName,
                degree: data.degree
            })
            .then(function(docRef) {
                console.log("Document written with ID:  ", docRef.id);
                app.ports.updatedUserInfo.send(true);
            })
            .catch(function(error) {
                //TODO implement
                app.ports.updateUserInfoError.send(error.code);
                console.log(error.code);
            });
        }
        else {
            db.collection("users").where("email", "==", user.email)
                .get()
                .then(function(querySnapshot) {
                    querySnapshot.forEach(function(doc) {
                        var userRef = db.collection("users").doc(doc.id);
                        return userRef.update({
                            firstName: data.firstName,
                            lastName: data.lastName,
                            degree: data.degree 
                        })
                        .then(function() {
                            app.ports.updatedUserInfo.send(true);
                            console.log("updated user info");
                        })
                        .catch(function(error) {
                            //TODO implement
                            app.ports.updateUserInfoError.send(error.code);
                            console.log(error.code);
                        });
                    });
                })
                .catch(function(error) {
                    console.log(error.code);
                });
        }
    }
});

app.ports.attemptSignOut.subscribe(function(data) {
    if (data.requestedLogOut) {
        firebase.auth().signOut()
            .then(function() {
                app.ports.signOutSucceeded.send(true);
            })
            .catch(function(error) {
                app.ports.signOutError.send(error);
                console.log(error.code);
            });
    }
});
