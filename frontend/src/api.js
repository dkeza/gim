const api = "/api/v1/index.prg";
const url = "" + api;

const APIGet = async (request) => {

    try {
        const res = await fetch(url, {
            method: "POST",
            mode: "cors",
            headers: {
                "Content-Type": "application/json"
            },
            body: JSON.stringify(request)
        });
        let r = await res.json();
        return Promise.resolve(r);
    } catch (err) {
        return Promise.reject(err);
    }

}

export default APIGet;