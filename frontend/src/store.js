import { writable } from 'svelte/store';

const s = writable(
    {
        userId: '',
        userName: ''
    },
);

export default s;