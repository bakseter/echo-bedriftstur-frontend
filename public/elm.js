var app = Elm.Main.init ({
    node: document.getElementById('elm'),
    flags: window.location.pathname
});

document.onload = function() {
    app.ports.start.send(true);
};

app.ports.logIn.subscribe(function(data) {
    var actionCodeSettings = {
        url : 'https://echobedriftstur.no/verified',
        handleCodeInApp : true
    };
    firebase.auth().sendSignInLinkToEmail(data.email, actionCodeSettings)
        .then(function() {
            window.localStorage.setItem('emailForSignIn', data.email);
        })
        .catch(function(error) {
            app.ports.sendSignInLinkError.send(error);
            console.log(error);
            console.log('error on sendSignInLinkToEmail');
        });
});

app.ports.verifyUser.subscribe(function(data) {
    if (firebase.auth().isSignInWithEmailLink(window.location.href)) {
        var email = window.localStorage.getItem('emailForSignIn');
        if (!email) {
            email = window.prompt('Vennligst skriv inn din mail for konfirmasjon');
        }

        firebase.auth().signInWithEmailLink(email, window.location.href)
            .then(function(result) {
                window.localStora.removeItem('emailForSignIn');
                apps.ports.logInSucceeded(result);
            })
            .catch(function(error) {
                app.ports.send.signInWithLinkError(error)
                console.log(error);
                console.log('error on signInWithEmailLink');
            });
    }
});
