import { Elm } from './Main.elm';
import { initialClientPorts } from './openTokClient';
import { initialMicroModal } from './modals';

const elmApp = Elm.Main.init({
    flags: flags,
    node: document.querySelector('#app')
});

initialClientPorts(elmApp);
initialMicroModal(elmApp);
