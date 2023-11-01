import { writable } from 'svelte/store';

const s = writable(
    {
        oUser: {
            nick: "",
            firstname: "",
            lastname: "",
            email: "",
            loggedIn: false
        },
        oGym: {
            id: 0,
            name: "",
            list: []
        },
        oEquipment: {
            list: []
        }
    },
);

export default s;