var app = Elm.Main.init ({
    node: document.getElementById("elm"),
    flags: window.location.pathname
});

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
            app.ports.sendSignInLinkError.send(error);
            console.log(error);
            console.log("error on sendSignInLinkToEmail");
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
            app.ports.logInSucceeded.send(result);
            console.log(result);
        })
        .catch(function(error) {
            app.ports.signInWithLinkError.send(error);
            console.log(error);
            console.log("error on signInWithEmailLink");
        });
}
