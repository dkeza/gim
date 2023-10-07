import { writable } from 'svelte/store';

const s = writable(
    {
        userId: '',
        userName: '',
        loggedIn: false
    },
);

export default s;