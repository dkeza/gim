<script>
    import { onMount } from "svelte";
    import { _ } from "./services/i18n";
    import APIGet from "./services/api";
    import s from "./services/stores.js";

    let name = "";

    onMount(async () => {
        List();
    });

    async function List() {
        try {
            const res = await APIGet({
                method: "gym_list",
            });

            console.log(res);

            if (res.success) {
                $s.oGym.list = [...res.list];
            }
        } catch (err) {
            console.log(err);
        }
    }

    async function Add() {
        let gymName = name;

        name = "";

        try {
            const res = await APIGet({
                method: "gym_create",
                name: gymName,
            });

            console.log(res);

            if (res.success) {
                List();
            }
        } catch (err) {
            console.log(err);
        }
    }
</script>

<h1>{$_("gyms")}</h1>
<h5>{$_("add_new_gym")}</h5>
<form on:submit|preventDefault={Add}>
    <input placeholder={$_("name")} bind:value={name} />
    <button type="submit">{$_("create")}</button>
</form>
<br />
<ul>
    {#each $s.oGym.list as item}
        <li>{item.name}</li>
    {/each}
</ul>
