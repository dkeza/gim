<script>
  import Router from "svelte-spa-router";
  import Home from "./Home.svelte";
  import About from "./About.svelte";
  import Login from "./Login.svelte";
  import Logout from "./Logout.svelte";
  import s from "./store.js";
</script>

<div class="pure-menu pure-menu-horizontal">
  <ul class="pure-menu-list">
    <li class="pure-menu-item"><a href="#/">Home</a></li>
    <li class="pure-menu-item"><a href="#/about">About</a></li>
    <li class="pure-menu-item">
      {#if $s.userId.length > 0}
        <a href="#/logout">Logout</a>
      {:else}
        <a href="#/login">Login</a>
      {/if}
    </li>
  </ul>
</div>

<Router
  routes={{
    "/": Home,
    "/about": About,
    "/login": Login,
    "/logout": Logout,
  }}
/>
