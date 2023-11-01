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
                method: "equipment_list",
            });

            console.log(res);

            if (res.success) {
                $s.oEquipment.list = [...res.list];
            }
        } catch (err) {
            console.log(err);
        }
    }

    async function Add() {
        let newName = name;

        name = "";

        try {
            const res = await APIGet({
                method: "equipment_create",
                name: newName,
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

<h1>{$_("equipments")}</h1>
<h5>{$_("add_new_equipment")}</h5>
<form on:submit|preventDefault={Add}>
    <input placeholder={$_("name")} bind:value={name} />
    <button type="submit">{$_("create")}</button>
</form>
<br />
<ul>
    {#each $s.oEquipment.list as item}
        <li>{item.name}</li>
    {/each}
</ul>
