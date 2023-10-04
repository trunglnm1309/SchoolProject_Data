import React, { useState } from "react";
import axiosClient from "../../../helper/axiosClient";
import * as actions from "./../../../actions/index";
import { useDispatch } from "react-redux";
import { toast } from "react-toastify";

function Register(props) {
    const dispatch = useDispatch();
    const { display, closeModal } = props;
    const [Email, setEmail] = useState("");
    function onChangeForm(e) {
        setEmail(e.target.value);
    }
    function clearForm() {
        setEmail("");
    }
    function onSubmitForgotPassword(e) {
        e.preventDefault();
        axiosClient({
            url: `/api/auth/forgot`,
            method: "post",
            data: {
                email: Email,
            },
        }).then((data) => {
            console.log(data);
            if (data.status === 200) {
                toast.success(data.msg);
            } else {
                toast.error(data.msg);
            }
        });
    }
    return (
        <form
            style={display === 3 ? { display: "block" } : { display: "none" }}
            onSubmit={onSubmitForgotPassword}
        >
            <div class="modal-main__title--container">
                <div
                    class="modal-main__title--container__child "
                    style={{ width: "100%" }}
                >
                    <h1 class="modal-main__title">Quên mật khẩu</h1>
                </div>
            </div>
            <div
                class="modal__body__sign-in__form modal__body__sign-in__form__DK"
                style={
                    display === 3 ? { display: "flex" } : { display: "none" }
                }
            >
                <input
                    type="email"
                    class="modal__body__sign-in__form-input"
                    name="email"
                    value={Email}
                    required
                    placeholder="Nhập Email"
                    onChange={onChangeForm}
                />
                <div class="modal__body__sign-in__form-button">
                    <button
                        class="modol__body__sign-in__form-button-back"
                        onClick={closeModal}
                    >
                        Trở lại
                    </button>
                    <button
                        type="submit"
                        class="modol__body__sign-in__form-button-signin"
                    >
                        Gửi
                    </button>
                </div>
            </div>
            <div class="modal__body__footer">
                <ul class="modal__body__footer__list">
                    <li class="modal__body__footer__list-item">
                        <a
                            href=""
                            class="modal__body__footer__list-item-link"
                            style={{ background: "rgb(34, 34, 138)" }}
                        >
                            <i class="fab fa-facebook-square"></i>
                            <span>Facebook</span>
                        </a>
                    </li>
                    <li class="modal__body__footer__list-item">
                        <a
                            href=""
                            class="modal__body__footer__list-item-link"
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
                        </a>
                    </li>
                </ul>
            </div>
        </form>
    );
}

export default Register;
