<script>
    import "./css/reset.css";
    import "./css/style.css";
    import s from "./services/stores.js";
    import Router from "svelte-spa-router";
    import { wrap } from "svelte-spa-router/wrap";
    import { push, link, location } from "svelte-spa-router";
    import { setupI18n, isLocaleLoaded, locale, _ } from "./services/i18n";
    import Home from "./Home.svelte";
    import Workouts from "./Workouts.svelte";
    import Equipments from "./Equipments.svelte";
    import Gyms from "./Gyms.svelte";
    import Login from "./Login.svelte";
    import Logout from "./Logout.svelte";
    import LocaleSelector from "./components/LocaleSelector.svelte";
    import "chota";
    import { Container, Row, Col } from "svelte-chota";

    $: {
        if (!$isLocaleLoaded) {
            $locale = "rs";
            setupI18n({ withLocale: "rs" });
        }
    }
</script>

<Container>
    <nav class="nav">
        <div class="nav-left">
            <div class="tabs">
                <a class:active={$location === "/"} href="/" use:link
                    >{$_("home")}</a
                >
                {#if !$s.oUser.loggedIn}
                    <a
                        class:active={$location === "/login"}
                        href="/login"
                        use:link>{$_("login")}</a
                    >
                {:else}
                    <a
                        class:active={$location === "/workouts"}
                        href="/workouts"
                        use:link>{$_("workouts")}</a
                    >
                    <a
                        class:active={$location === "/gyms"}
                        href="/gyms"
                        use:link>{$_("gyms")}</a
                    >
                    <a
                        class:active={$location === "/equipments"}
                        href="/equipments"
                        use:link>{$_("equipments")}</a
                    >
                    <a
                        class:active={$location === "/logout"}
                        href="/logout"
                        use:link>{$_("logout")}</a
                    >
                {/if}
                {#if $s.oUser.loggedIn}
                    <span class="user_info"
                        >{$_("hi") + " " + $s.oUser.nick}</span
                    >
                {/if}
            </div>
        </div>
    </nav>

    <Row>
        <Col size="8" sizeMD="8" sizeLG="8">
            <main>
                <Router
                    routes={{
                        "/": wrap({
                            component: Home,
                            conditions: [
                                () => {
                                    return true;
                                },
                            ],
                        }),
                        "/workouts": wrap({
                            component: Workouts,
                            conditions: [
                                () => {
                                    if (!$s.oUser.loggedIn) {
                                        push("/login");
                                        return false;
                                    }
                                    return true;
                                },
                            ],
                        }),
                        "/gyms": wrap({
                            component: Gyms,
                            conditions: [
                                () => {
                                    if (!$s.oUser.loggedIn) {
                                        push("/login");
                                        return false;
                                    }
                                    return true;
                                },
                            ],
                        }),
                        "/equipments": wrap({
                            component: Equipments,
                            conditions: [
                                () => {
                                    if (!$s.oUser.loggedIn) {
                                        push("/login");
                                        return false;
                                    }
                                    return true;
                                },
                            ],
                        }),
                        "/login": wrap({
                            component: Login,
                            conditions: [
                                () => {
                                    if ($s.oUser.loggedIn) {
                                        push("/");
                                        return false;
                                    }
                                    return true;
                                },
                            ],
                        }),
                        "/logout": wrap({
                            component: Logout,
                            conditions: [
                                () => {
                                    if (!$s.oUser.loggedIn) {
                                        push("/login");
                                        return false;
                                    }
                                    return true;
                                },
                            ],
                        }),
                    }}
                />
            </main>
        </Col>
        <Col size="4" sizeMD="4" sizeLG="4">
            <aside>
                <div class="nav-right langselect">
                    <LocaleSelector
                        on:locale-changed={(e) =>
                            setupI18n({ withLocale: e.detail })}
                    />
                </div>
            </aside>
        </Col>
    </Row>

    <footer>(C) 2023 by Keza</footer>
</Container>

<style>
    .langselect {
        padding-bottom: 4px;
    }
</style>
