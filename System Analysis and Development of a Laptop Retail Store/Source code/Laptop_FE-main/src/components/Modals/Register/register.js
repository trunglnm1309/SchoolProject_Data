import React, { useState, useEffect } from "react";
import axiosClient from "../../../helper/axiosClient";
import * as actions from "./../../../actions/index";
import * as action from "./../../../actions/user";
import { useDispatch } from "react-redux";
import { toast } from "react-toastify";
import FacebookLogin from "react-facebook-login/dist/facebook-login-render-props";
import GoogleLogin from "react-google-login";

function Register(props) {
    const dispatch = useDispatch();
    const { display, closeModal } = props;
    useEffect(() => {
        clearForm();
    }, [display]);
    const [valueForm, setvalueForm] = useState({
        username: null,
        password: null,
        repassword: null,
        email: null,
        phone: null,
    });
    function onChangeForm(e) {
        setvalueForm({
            ...valueForm,
            [e.target.name]: e.target.value,
        });
    }
    function clearForm() {
        setvalueForm({
            username: "",
            password: "",
            repassword: "",
            email: "",
            phone: "",
        });
    }
    async function regiterUser(data) {
        var Data = await axiosClient({
            url: `/api/auth/register`,
            method: "post",
            data: {
                username: data.username,
                password: data.password,
                email: data.email,
                phone: data.phone,
            },
        });
        if (Data.statusCode === 200) {
            toast.success(Data.msg);
            toast.success("Truy cập vào Email để kích hoạt tài khoản");
            closeModal();
            clearForm();
        } else {
            toast.error(Data.msg);
        }
    }
    function validateForm() {
        if (valueForm.username.length <= 5) {
            toast.error("Tên tài khoản phải trên 6 ký tự");
            clearForm();
            return false;
        }
        if (valueForm.password != valueForm.repassword) {
            toast.error("Mật khẩu xác nhận không đúng");
            clearForm();
            return false;
        }
        if (valueForm.password.length <= 5) {
            toast.error("Mật khẩu phải trên 5 ký tự");
            clearForm();
            return false;
        }
        return true;
    }
    function onSubmitRegister(e) {
        e.preventDefault();
        if (validateForm()) {
            regiterUser(valueForm);
        }
    }
    function onClickForgotPassword() {
        dispatch(actions.showForgotPassword());
    }
    function responseFacebook(response) {
        axiosClient({
            url: `/api/auth/register/facebook`,
            method: "post",
            data: {
                username: response.email,
                name: response.name,
                password: response.userID,
                email: response.email,
                image: response.picture.data.url,
            },
        }).then((data) => {
            if (data.statusCode === 200) {
                axiosClient({
                    url: `/api/auth/login`,
                    method: "post",
                    data: {
                        username: response.email,
                        password: response.userID,
                    },
                }).then((dataResponse) => {
                    localStorage.setItem("Authorization", dataResponse.jwt);
                    dispatch(action.getApiUser());
                    closeModal();
                    clearForm();
                });
            } else {
                toast.error(data.msg);
            }
        });
    }
    function responseGoogle(response) {
        axiosClient({
            url: `/api/auth/register/facebook`,
            method: "post",
            data: {
                username: response.profileObj.name,
                name: response.profileObj.name,
                password: response.profileObj.googleId,
                email: response.profileObj.email,
                image: response.profileObj.imageUrl,
            },
        }).then((data) => {
            if (data.statusCode === 200) {
                axiosClient({
                    url: `/api/auth/login`,
                    method: "post",
                    data: {
                        username: response.profileObj.name,
                        password: response.profileObj.googleId,
                    },
                }).then((dataResponse) => {
                    localStorage.setItem("Authorization", dataResponse.jwt);
                    dispatch(action.getApiUser());
                    closeModal();
                    clearForm();
                });
            } else {
                toast.error(data.msg);
            }
        });
    }
    return (
        <form
            style={display === 2 ? { display: "block" } : { display: "none" }}
            onSubmit={onSubmitRegister}
        >
            <div className="modal-main__title--container">
                <div className="modal-main__title--container__child modal-main__title--container__child--active">
                    <h1 className="modal-main__title">Đăng ký</h1>
                </div>
                <div
                    className="modal-main__title--container__child"
                    onClick={() => dispatch(actions.showLogin())}
                >
                    <h1 className="modal-main__title">Đăng nhập</h1>
                </div>
            </div>
            <div
                className="modal__body__sign-in__form modal__body__sign-in__form__DK"
                style={
                    display === 2 ? { display: "flex" } : { display: "none" }
                }
            >
                <input
                    type="text"
                    className="modal__body__sign-in__form-input"
                    name="username"
                    value={valueForm.username}
                    required
                    placeholder="Tên người dùng"
                    onChange={onChangeForm}
                />
                <input
                    type="password"
                    className="modal__body__sign-in__form-input"
                    name="password"
                    value={valueForm.password}
                    required
                    placeholder="Mật Khẩu"
                    onChange={onChangeForm}
                />
                <input
                    type="password"
                    className="modal__body__sign-in__form-input"
                    name="repassword"
                    value={valueForm.repassword}
                    required
                    placeholder="Nhập Lại Mật Khẩu"
                    onChange={onChangeForm}
                />
                <input
                    type="email"
                    className="modal__body__sign-in__form-input"
                    name="email"
                    value={valueForm.email}
                    required
                    placeholder="Nhập Email"
                    onChange={onChangeForm}
                />
                <input
                    type="text"
                    className="modal__body__sign-in__form-input"
                    name="phone"
                    value={valueForm.phone}
                    required
                    placeholder="Nhập Số Điện Thoại"
                    onChange={onChangeForm}
                />
                <div className="modal__body__sign-in__form-support">
                    <span
                        className="modal__body__sign-in__form-helps__forget"
                        onClick={onClickForgotPassword}
                    >
                        Quên Mật Khẩu
                    </span>
                    <span className="modal__body__sign-in__form-helps__help">
                        Cần Trợ giúp?
                    </span>
                </div>
                <div className="modal__body__sign-in__form-button">
                    <input
                        type="button"
                        value="Trở lại"
                        className="modol__body__sign-in__form-button-back"
                        onClick={closeModal}
                    />
                    <input
                        type="submit"
                        value="Đăng ký"
                        className="modol__body__sign-in__form-button-signin"
                    />
                </div>
            </div>
            <div className="modal__body__footer">
                <ul className="modal__body__footer__list">
                    <FacebookLogin
                        appId="600344331360290"
                        autoLoad
                        callback={responseFacebook}
                        fields="name,email,picture"
                        render={(renderProps) => (
                            <li
                                className="modal__body__footer__list-item"
                                onClick={renderProps.onClick}
                            >
                                <span
                                    className="modal__body__footer__list-item-link"
                                    style={{ background: "rgb(34, 34, 138)" }}
                                >
                                    <i className="fab fa-facebook-square"></i>
                                    <span>Facebook</span>
                                </span>
                            </li>
                        )}
                    />
                    <GoogleLogin
                        clientId="280792067113-kv8rna3l8b03a0cut9hegdqfe39ag6h7.apps.googleusercontent.com"
                        render={(renderProps) => (
                            <li
                                className="modal__body__footer__list-item"
                                onClick={renderProps.onClick}
                            >
                                <span
                                    className="modal__body__footer__list-item-link"
                                    style={{
                                        background: "#fff",
                                        color: "black",
                                        display: "flex;",
                                    }}
                                >
                                    <img
                                        src="./img/google.png"
                                        style={{
                                            width: "20px",
                                            objectFit: "cover",
                                            marginRight: "30px",
                                        }}
                                        alt=""
                                    />
                                    <span>Google</span>
                                </span>
                            </li>
                        )}
                        buttonText="Login"
                        onSuccess={responseGoogle}
                        onFailure={responseGoogle}
                        cookiePolicy={"single_host_origin"}
                    />
                </ul>
            </div>
        </form>
    );
}

export default Register;
