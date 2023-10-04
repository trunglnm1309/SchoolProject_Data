import React, { useEffect, useState } from "react";
import Breadcum from "../../components/Breadcum/Breadcum";
import { toast } from "react-toastify";
import AxiosClient from "../../helper/axiosClient";
import "./ForgotPassword.css";
function ForgotPassword(props) {
    const { history } = props;
    const { id } = props.match.params;
    const [value, setvalue] = useState({
        password: "",
        repeatPassword: "",
    });
    function handleChange(e) {
        setvalue({
            ...value,
            [e.target.name]: e.target.value,
        });
    }
    function handleSubmit(e) {
        e.preventDefault();
        if (value.password != value.repeatPassword) {
            toast.error("Mật khẩu và mật khẩu nhập lại không giống nhau");
        } else {
            AxiosClient({
                url: `/api/auth/forgot/${id}`,
                method: "post",
                data: {
                    password: value.password,
                },
            }).then((data) => {
                console.log(data);
                if (data.status != 200) {
                    toast.error("Đổi mật khẩu không thành công");
                } else {
                    toast.success("Đổi mật khẩu thành công");
                    history.push("/");
                }
            });
        }
    }
    useEffect(() => {
        AxiosClient({
            url: `/api/auth/forgot/${id}`,
            method: "get",
        }).then((data) => {
            if (data.status != 200) {
                history.push("/");
            }
        });
    });
    return (
        <div className="container">
            <Breadcum final="Quên mật khẩu" />
            <div className="login_register_wrap register">
                <div className="container">
                    <div className="row justify-content-center">
                        <div className="col-xl-6 col-md-10">
                            <div className="login_wrap">
                                <div className="register-wrap">
                                    <div className="heading_s1 mb-4">
                                        <h3>Đổi mật khẩu</h3>
                                    </div>
                                    <form method="POST" onSubmit={handleSubmit}>
                                        <div className="form-group">
                                            <input
                                                type="password"
                                                className="form-control"
                                                onChange={handleChange}
                                                name="password"
                                                placeholder="Mật khẩu mới"
                                            />
                                            <br></br>
                                        </div>
                                        <div className="form-group">
                                            <input
                                                type="password"
                                                className="form-control"
                                                onChange={handleChange}
                                                name="repeatPassword"
                                                placeholder="Nhập lại mật khẩu "
                                            />
                                        </div>
                                        <div className="form-group">
                                            <button
                                                type="submit"
                                                className="btn btn-primary btn-block p-3 mt-5"
                                                name="register"
                                            >
                                                Đổi mật khẩu
                                            </button>
                                        </div>
                                    </form>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}

export default ForgotPassword;
