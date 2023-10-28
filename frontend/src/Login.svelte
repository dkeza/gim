<script>
    import { _ } from "./services/i18n";
    import s from "./services/stores.js";
    import APIGet from "./services/api";
    import { push } from "svelte-spa-router";

    let errorMessage = "";
    let errorCode = 0;
    let register = false;
    let oUser = {
        method: "",
        nick: "",
        firstname: "",
        lastname: "",
        email: "",
        password: "",
    };

    async function tryLogin() {
        oUser.method = "login";

        $s.oUser.nick = "";
        $s.oUser.firstname = "";
        $s.oUser.lastname = "";
        $s.oUser.email = "";
        $s.oUser.loggedIn = false;

        const res = await APIGet(oUser);

        console.log(res);

        if (res.success) {
            $s.oUser.nick = res.nick;
            $s.oUser.firstname = res.firstname;
            $s.oUser.lastname = res.lastname;
            $s.oUser.email = res.email;
            $s.oUser.loggedIn = true;
            $s.oGym.id = res.gym.id;
            $s.oGym.name = res.gym.name;
            $s.oGym.list = res.gym.list;
            push("/");
            return;
        }

        errorMessage = res.error;
        errorCode = res.errorcode;
    }

    async function tryRegister() {
        oUser.method = "register";
        const res = await APIGet(oUser);

        console.log(res);

        if (res.success) {
            $s.oUser.nick = res.nick;
            $s.oUser.firstname = res.firstname;
            $s.oUser.lastname = res.lastname;
            $s.oUser.email = res.email;
            $s.oUser.loggedIn = true;
            push("/");
            return;
        }

        errorMessage = res.error;
        errorCode = res.errorcode;
    }

    function showRegister() {
        register = true;
    }

    function showLogin() {
        register = false;
    }
</script>

<h1>{$_("login")}</h1>

<h1>{$s.oUser.firstname + " " + $s.oUser.firstname}</h1>

{#if errorMessage.length > 0}
    <p>{$_(errorMessage)} ({errorCode})</p>
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
        <input placeholder={$_("user")} bind:value={oUser.nick} />
        <input placeholder={$_("password")} bind:value={oUser.password} />
        <button type="submit">{$_("login")}</button>
    </form>
    <br />
    <form on:submit|preventDefault={showRegister}>
        <p>{$_("no_account_register")}</p>
        <button type="submit">{$_("register")}</button>
    </form>
{/if}
