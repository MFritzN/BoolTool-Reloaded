import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import * as bulmaToast from 'bulma-toast'

const basePath = new URL(process.env.ELM_APP_BASE_URL).pathname;
console.log(basePath);

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {basePath},
});

app.ports.copy.subscribe((string) => {
  navigator.clipboard.writeText(string).then(() => toast("Copied", "is-success")
    , () => toast("Copy failed", "is-danger"));
})

app.ports.share.subscribe(() => {
  navigator.clipboard.writeText(document.URL).then(() => toast("Link Copied", "is-success")
    , () => toast("Copy failed\n You can just copy the URL.", "is-danger"));
})
/** Make sure, the cursor does not jump to the end when elm live edits an input box.
 * Code was taken from https://discourse.elm-lang.org/t/modifying-the-string-in-a-text-box-pushes-the-cursor-to-the-end/6151/6
*/
window.addEventListener("input", event => {

  const target = event.target
  const { value, selectionStart } = target

  if (target.classList.contains("avoid-cursor-jump")) {
    // wait for Elm to decide what the new value
    // of the input actually should look like
    requestAnimationFrame(() => {
      const newValue = target.value
      if (value !== newValue) {
        target.selectionEnd = target.selectionStart =
          selectionStart - (value.length - newValue.length)
      }
    })
  }
}, true)

function toast(text, type) {
  bulmaToast.toast({
    message: text,
    type: type,
    dismissible: true,
    animate: { in: 'fadeIn', out: 'fadeOut' },
  })
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
