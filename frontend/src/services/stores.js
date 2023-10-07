import { writable } from 'svelte/store';

const s = writable(
    {
        oUser: {
            nick: "",
            firstname: "",
            lastname: "",
            email: "",
            loggedIn: false
        }
    },
);

export default s;