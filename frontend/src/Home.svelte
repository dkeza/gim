<script>
    import { _ } from "./services/i18n";
    import { push } from "svelte-spa-router";
    import s from "./services/stores.js";

    function goLoginPage() {
        push("/login");
    }
</script>

<h3>{$_("welcome")}</h3>

{#if $s.loggedIn}
    <h4>{$s.userName}</h4>
{:else}
    <button on:click={goLoginPage}>{$_("login")}</button>
{/if}
