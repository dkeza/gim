import { derived } from 'svelte/store';
import { dictionary, locale, _ } from 'svelte-i18n';

const MESSAGE_FILE_URL_TEMPLATE = '/lang/{locale}.json';

let cachedLocale;

function setupI18n({ withLocale: _locale } = { withLocale: 'de' }) {
    const messsagesFileUrl = MESSAGE_FILE_URL_TEMPLATE.replace('{locale}', _locale);

    return fetch(messsagesFileUrl)
        .then(response => response.json())
        .then((messages) => {
            dictionary.set({ [_locale]: messages });

            cachedLocale = _locale;

            locale.set(_locale);
        });
}

function formatDate(date, options) {
    return new Intl.DateTimeFormat(cachedLocale, options)
        .format(new Date(date));
}

const isLocaleLoaded = derived(locale, $locale => typeof $locale === 'string');
const localeDescription = derived(locale, $locale => $locale === "en" ? "English" : $locale === "nl" ? "Nederlands" : $locale === "rs" ? "Srpski" : "Deutsch");

const rs = {
    days: ['Nedelja', 'Ponedeljak', 'Utorak', 'Sreda', 'Četvrtak', 'Petak', 'Subota', 'Nedelja'],
    daysShort: ['Ned', 'Pon', 'Uto', 'Sre', 'Čet', 'Pet', 'Sub', 'Ned'],
    daysMin: ['Ne', 'Po', 'Ut', 'Sr', 'Če', 'Pe', 'Su', 'Ne'],
    months: ['Januar', 'Februar', 'Mart', 'April', 'Maj', 'Jun', 'Jul', 'Avgust', 'Septembar', 'Oktobar', 'Novembar', 'Decembar'],
    monthsShort: ['Jan', 'Feb', 'Mar', 'Apr', 'Maj', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dec'],
    meridiem: ['pre', 'pos'],
    suffix: ['prvi', 'drugi', 'treći', 'četvrti'],
    todayBtn: 'Danas',
    clearBtn: 'Poništi',
    timeView: 'Prikaži vreme',
    backToDate: 'Nazad na kalendar'
}

const nl = {
    days: ['Zondag', 'Maandag', 'Dinsdag', 'Woensdag', 'Donderdag', 'Vrijdag', 'Zaterdag', 'Zondag'],
    daysShort: ['Zon', 'Maa', 'Din', 'Woe', 'Don', 'Vri', 'Zat', 'Zon'],
    daysMin: ['Zo', 'Ma', 'Di', 'Wo', 'Do', 'Vr', 'Za', 'Zo'],
    months: ['Januari', 'Februari', 'Maart', 'April', 'Mei', 'Juni', 'Juli', 'Augustus', 'September', 'Oktober', 'November', 'December'],
    monthsShort: ['Jan', 'Feb', 'Maa', 'Apr', 'Mei', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dec'],
    meridiem: ['am', 'pm'],
    suffix: ['ee', 'de', 'de', 'vi'],
    todayBtn: 'Vandaag',
    clearBtn: 'Ongedaan maken',
    timeView: 'Tijdweergave tonen',
    backToDate: 'Terug naar kalenderweergave'
}

export { _, locale, setupI18n, formatDate, isLocaleLoaded, localeDescription, rs, nl };
