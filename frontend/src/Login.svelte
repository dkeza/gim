<script>
    import { _ } from "./services/i18n";
    import s from "./services/stores.js";
    import APIGet from "./services/api";
    import { push } from "svelte-spa-router";
    let u = "";
    let p = "";
    let e = "";

    async function tryLogin() {
        const user = u,
            password = p;

        u = "";
        p = "";
        e = "";
        const res = await APIGet({
            method: "login",
            user: user,
            password: password,
        });
        console.log(res);
        if (res.success) {
            $s.userId = res.user;
            $s.userName = res.name;
            $s.loggedIn = true;
            push("/");
            return;
        } else {
            $s.userId = "";
            $s.userName = "";
            $s.loggedIn = false;
        }

        e = res.error;
    }
</script>

<h1>{$_("login")}</h1>

<h1>{$s.userName}</h1>

{#if e.length > 0}
    <p>{e}</p>
{/if}

<form on:submit|preventDefault={tryLogin}>
    <input placeholder={$_("user")} bind:value={u} />
    <input placeholder={$_("password")} bind:value={p} />
    <button type="submit">{$_("login")}</button>
</form>
