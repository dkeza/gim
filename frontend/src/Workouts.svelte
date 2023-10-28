<script>
    import { _ } from "./services/i18n";
    import s from "./services/stores.js";
    import APIGet from "./services/api";
    import { Button } from "svelte-chota";

    async function select(GymID) {
        try {
            const res = await APIGet({
                method: "user_select_gym",
                id: GymID,
            });

            console.log(res);

            if (res.success) {
                $s.oGym.id = res.id;
                $s.oGym.name = res.name;
            }
        } catch (err) {
            console.log(err);
        }
    }
</script>

<h3>{$_("workouts")}</h3>

<Button dropdown={$s.oGym.name} autoclose outline>
    {#each $s.oGym.list as item}
        <p>
            <a href={null} on:click|preventDefault={() => select(item.id)}
                >{item.name}</a
            >
        </p>
    {/each}
</Button>
