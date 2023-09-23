<script>
  import s from "./store.js";
  import APIGet from "./api";
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
      return;
    }

    e = res.error;
  }
</script>

<h1>Login</h1>

<h1>{$s.userName}</h1>

{#if e.length > 0}
  <p>{e}</p>
{/if}

<form on:submit|preventDefault={tryLogin}>
  <input placeholder="user" bind:value={u} />
  <input placeholder="password " bind:value={p} />
  <button type="submit">Login</button>
</form>
