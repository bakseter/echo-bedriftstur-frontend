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

const app = Elm.Main.init ({
    node: document.getElementById("elm")
});
