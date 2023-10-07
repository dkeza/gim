<script>
    import { _ } from "./services/i18n";
    import s from "./services/stores.js";
    import APIGet from "./services/api";
    import { push } from "svelte-spa-router";
    let u = "";
    let p = "";
    let e = "";
    let register = false;
    let oUser = {
        method: "register",
        nick: "",
        firstname: "",
        lastname: "",
        email: "",
        password: "",
    };

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
            $s.userId = res.nick;
            $s.userName = res.firstname + " " + res.lastname;
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

    async function tryRegister() {
        const res = await APIGet(oUser);

        console.log(res);

        if (res.success) {
            $s.userId = res.nick;
            $s.userName = res.firstname + " " + res.lastname;
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

    function showRegister() {
        register = true;
    }

    function showLogin() {
        register = false;
    }
</script>

<h1>{$_("login")}</h1>

<h1>{$s.userName}</h1>

{#if e.length > 0}
    <p>{e}</p>
{/if}

{#if register}
    <form on:submit|preventDefault={tryRegister}>
        <input placeholder={$_("user")} bind:value={oUser.nick} />
        <input placeholder={$_("first_name")} bind:value={oUser.firstname} />
        <input placeholder={$_("last_name")} bind:value={oUser.lastname} />
        <input placeholder={$_("email")} bind:value={oUser.email} />
        <input placeholder={$_("password")} bind:value={oUser.password} />
        <button type="submit">{$_("register")}</button>
    </form>
    <br />
    <form on:submit|preventDefault={showLogin}>
        <p>{$_("you_have_account")}</p>
        <button type="submit">{$_("login")}</button>
    </form>
{:else}
    <form on:submit|preventDefault={tryLogin}>
        <input placeholder={$_("user")} bind:value={u} />
        <input placeholder={$_("password")} bind:value={p} />
        <button type="submit">{$_("login")}</button>
    </form>
    <br />
    <form on:submit|preventDefault={showRegister}>
        <p>{$_("no_account_register")}</p>
        <button type="submit">{$_("register")}</button>
    </form>
{/if}
