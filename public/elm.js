var app = Elm.Main.init ({
    node: document.getElementById("elm"),
    flags: window.location.pathname
});

app.ports.logIn.subscribe(function(data) {
    firebase.auth().signInWithEmailAndPassword(data.email, data.password)
        .then(function(result) {
            var user = firebase.auth().currentUser;
            user.updateProfile({
                displayName : data.username
            }).then(function() {
                app.ports.logInInfo.send(user.displayName);
            }).catch(function(error) {
                console.log(error);
            });
        }).catch(function(error) {
            console.log(error.code);
            console.log(error.message)
    });
});
