<script>
    import { onMount } from "svelte";
    import { _ } from "./services/i18n";
    import APIGet from "./services/api";
    import { push } from "svelte-spa-router";

    let gymsList = [];

    onMount(async () => {
        try {
            const res = await APIGet({
                method: "gym_list",
            });

            console.log(res);

            if (res.success) {
                gymsList = [...res.list];
                return;
            }
        } catch (err) {
            console.log(err);
        }
    });
</script>

<h1>{$_("gyms")}</h1>

<ul>
    {#each gymsList as item}
        <li>{item.name}</li>
    {/each}
</ul>
