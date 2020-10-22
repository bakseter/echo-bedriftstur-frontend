import * as firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/firestore';

import './main.css';
import { Elm } from './Main.elm';

const firebaseConfig =
  { apiKey: process.env.ELM_APP_API_KEY
  , authDomain: process.env.ELM_APP_AUTH_DOMAIN
  , databaseURL: process.env.ELM_APP_DATABASE_URL
  , projectId: process.env.ELM_APP_PROJECT_ID
  , storageBucket: process.env.ELM_APP_STORAGE_BUCKET
  , messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID
  , appId: process.env.ELM_APP_APP_ID
  , measurementId: process.env.ELM_APP_MEASUREMENT_ID
};
firebase.initializeApp(firebaseConfig);

const app = Elm.Main.init ({
    node: document.getElementById('elm'),
    flags: firebaseConfig.apiKey
});
