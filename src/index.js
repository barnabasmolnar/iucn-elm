import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    token: process.env.TOKEN,
    apiUrl: process.env.API_URL,
    viewLimit: process.env.VIEW_LIMIT,
  },
});
