<script>
    import { _ } from "./services/i18n";
    import s from "./services/stores.js";
    import APIGet from "./services/api";
    import { push } from "svelte-spa-router";

    let e = "";

    async function tryLogout() {
        const res = await APIGet({
            method: "logout",
        });
        console.log(res);
        if (res.success) {
            $s.userId = "";
            $s.userName = "";
            $s.loggedIn = false;
            push("/");
            return;
        }

        e = res.error;
    }
</script>

<h1>{$_("logout")}</h1>

{#if e.length > 0}
    <p>{e}</p>
{/if}

<form on:submit|preventDefault={tryLogout}>
    <button type="submit">{$_("logout")}</button>
</form>
