import axios from "axios";

function getLocalToken() {
    const token = localStorage.getItem("Authorization");
    return token;
}

// console.log("BASE_URL :>> ", process.env.REACT_APP_BASE_URL);

const AxiosInstance = axios.create({
    baseURL: process.env.REACT_APP_BASE_URL || "",
    // headers: {
    //     "Content-Type": "application/json",
    // },
});

AxiosInstance.interceptors.request.use(
    async (config) => {
        const token = getLocalToken();
        if (token) {
            config.headers.Authorization = token;
        }
        return config;
    },
    (err) => {
        return Promise.reject(err);
    }
);
AxiosInstance.interceptors.response.use(
    (response) => {
        if (response && response.data) {
            return response.data;
        }
        return response;
    },
    (err) => {
        localStorage.removeItem("Authorization");
        alert(
            "Phiên đăng nhập đã hết hạn! vui lòng đăng nhập lại để tiếp tục..."
        );
        //    window.location.href="/";
        throw err;
    }
);
export default AxiosInstance;
