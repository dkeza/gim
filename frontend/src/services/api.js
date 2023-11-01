const api = "/api/v1/index.prg";
const url = "" + api;

const APIGet = async (request) => {
    try {
        let r = null;
        const res = await fetch(url, {
            method: "POST",
            mode: "cors",
            headers: {
                "Content-Type": "application/json"
            },
            body: JSON.stringify(request)
        });

        console.log("api", res);

        if (res.ok) {
            r = await res.json();
        } else {
            r = { error: res.statusText, errorcode: res.status, success: false };
        }
        return Promise.resolve(r);
    } catch (err) {
        return Promise.reject(err.message || 'Server not available');
    }
}

export default APIGet;
