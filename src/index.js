import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

Elm.Main.init({
  node: document.getElementById('root')
});

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

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
