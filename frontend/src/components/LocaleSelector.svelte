<script>
    import { createEventDispatcher } from "svelte";
    import {localeDescription} from "./../services/i18n";
    import {Button} from "svelte-chota";

    const dispatch = createEventDispatcher();

    const handleLocaleChange = (lang) => {dispatch("locale-changed", lang)};

</script>

<Button dropdown={$localeDescription} autoclose outline>
    <p><a href={null} on:click|preventDefault={() => handleLocaleChange("de")}>Deutsch</a></p>
    <p><a href={null} on:click|preventDefault={() => handleLocaleChange("en")}>English</a></p>
    <p><a href={null} on:click|preventDefault={() => handleLocaleChange("nl")}>Nederlands</a></p>
    <p><a href={null} on:click|preventDefault={() => handleLocaleChange("rs")}>Srpski</a></p>
</Button>