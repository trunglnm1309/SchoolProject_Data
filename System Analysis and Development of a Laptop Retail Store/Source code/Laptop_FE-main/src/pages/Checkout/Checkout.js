import React, { useEffect, useState } from "react";
import Local from "../../assets/img/local.json";
import "./Checkout.css";
import { useHistory } from "react-router-dom";
import { useSelector, useDispatch } from "react-redux";
import * as action from "../../actions/index";
import AxiosClient from "../../helper/axiosClient";
import { toast } from "react-toastify";
function Checkout() {
    const history = useHistory();
    const dispatch = useDispatch();
    const cart = useSelector((state) => state.cart);
    const [formPayment, setformPayment] = useState({
        thanhpho: "",
        quanhuyen: "",
        phuongxa: "",
        sonha: "",
        payment: 1,
    });
    const [keylocal, setkeylocal] = useState({
        city: null,
        district: null,
    });
    function onHandleChange(e) {
        setformPayment({
            ...formPayment,
            [e.target.name]: e.target.value,
        });
        if (e.target.name === "thanhpho") {
            setkeylocal({
                ...keylocal,
                city: parseInt(
                    e.target[e.target.selectedIndex].getAttribute("data-id")
                ),
            });
        } else if (e.target.name === "quanhuyen") {
            setkeylocal({
                ...keylocal,
                district: parseInt(
                    e.target[e.target.selectedIndex].getAttribute("data-id")
                ),
            });
        }
    }
    function handlingFormCity() {
        var result = null;
        result = Local.map((city, index) => {
            return (
                <option value={city.name} data-id={index} key={index}>
                    {city.name}
                </option>
            );
        });
        return result;
    }
    function renderTotalMoney() {
        var total = 0;
        if (cart.length > 0) {
            cart.forEach((item) => {
                total += item.soluong * item.newprice;
            });
        }
        return total;
    }
    function handlingFormDistrict() {
        var result = null;
        if (keylocal.city != null) {
            result = Local[keylocal.city].districts.map((district, index) => {
                return (
                    <option value={district.name} data-id={index} key={index}>
                        {district.name}
                    </option>
                );
            });
        }
        return result;
    }
    function handlingFormWards() {
        var result = null;
        if (keylocal.district != null) {
            result = Local[keylocal.city].districts[
                keylocal.district
            ].wards.map((district, index) => {
                return (
                    <option value={district.name} data-id={index} key={index}>
                        {district.name}
                    </option>
                );
            });
        }
        return result;
    }
    function renderAddress() {
        return `${formPayment.sonha}-${formPayment.phuongxa}-${formPayment.quanhuyen}-${formPayment.thanhpho} `;
    }
    function onHandleSubmit(e) {
        console.log(formPayment);

        e.preventDefault();
        if (formPayment.payment === "2") {
            AxiosClient({
                url: "/api/auth/payment/create",
                method: "post",
                data: {
                    address: renderAddress(),
                    products: cart,
                    payment: formPayment.payment,
                    price: renderTotalMoney(),
                },
            }).then((data) => {
                if (data.code === "00") {
                    window.location.href = data.data;
                }
            });
        } else {
            AxiosClient({
                url: "/api/auth/order",
                method: "post",
                data: {
                    address: renderAddress(),
                    products: cart,
                    payment: formPayment.payment,
                },
            }).then((data) => {
                if (data.statusCode === 200) {
                    toast.success(data.msg);
                    dispatch(action.deleteAllCart());
                    history.push("/user/order");
                } else {
                    toast.erorr(data.msg);
                    history.push("/");
                }
            });
        }
    }

    return (
        <div className="container ">
            <div className="cart__cupon payment__info">
                <h2 className="cart__cupon-qr text-center">
                    Thông tin thanh toán
                </h2>
                <form className="payment__form" onSubmit={onHandleSubmit}>
                    <div className="row justify-content-center">
                        <div className="col-lg-8 text-center">
                            <h3 className="payment__form__title my-4">
                                Tỉnh/Thành phố
                            </h3>
                            <select
                                className="cart__cupon-type-input payment__form--input"
                                onChange={onHandleChange}
                                name="thanhpho"
                                value={formPayment.thanhpho}
                                required
                            >
                                <option disabled selected value="">
                                    Tỉnh/Thành phố
                                </option>
                                {handlingFormCity()}
                            </select>
                            <h3 className="payment__form__title my-4">
                                Quận/Huyện
                            </h3>
                            <select
                                className="cart__cupon-type-input payment__form--input"
                                disabled={
                                    formPayment.thanhpho === ""
                                        ? "disabled"
                                        : null
                                }
                                value={formPayment.quanhuyen}
                                onChange={onHandleChange}
                                name="quanhuyen"
                                required
                            >
                                <option disabled selected value="">
                                    {" "}
                                    Quận/Huyện
                                </option>
                                {handlingFormDistrict()}
                            </select>
                            <h3 className="payment__form__title my-4">
                                Phường/Xã
                            </h3>
                            <select
                                className="cart__cupon-type-input payment__form--input"
                                disabled={
                                    formPayment.quanhuyen === ""
                                        ? "disabled"
                                        : null
                                }
                                value={formPayment.phuongxa}
                                onChange={onHandleChange}
                                name="phuongxa"
                                required
                            >
                                <option disabled selected value="">
                                    {" "}
                                    Phường/Xã
                                </option>
                                {handlingFormWards()}
                            </select>
                            <h3 className="payment__form__title my-4">
                                Ấp/Số Nhà/Tên Đường
                            </h3>
                            <textarea
                                className="cart__cupon-type-input payment__form--input"
                                name="sonha"
                                id=""
                                cols="30"
                                rows="5"
                                onChange={onHandleChange}
                                placeholder="địa chỉ cụ thể: ấp, số nhà, tên đường"
                            ></textarea>
                            <h3 className="payment__form__title my-4">
                                Chọn phương thức thanh toán
                            </h3>
                            <select
                                className="cart__cupon-type-input payment__form--input"
                                disabled={
                                    formPayment.quanhuyen === ""
                                        ? "disabled"
                                        : null
                                }
                                value={formPayment.payment}
                                onChange={onHandleChange}
                                name="payment"
                                required
                            >
                                <option disabled selected value="">
                                    {" "}
                                    Chọn phương thức thanh toán
                                </option>
                                <option value="1">
                                    Thanh toán khi nhận hàng
                                </option>
                                <option value="2"> Thanh toán bằng card</option>
                            </select>
                            <div className="btn__payment_contain">
                                <button
                                    className="payment__btn__submit"
                                    type="submit"
                                >
                                    Thanh toán
                                </button>
                            </div>
                        </div>
                    </div>
                </form>
            </div>
        </div>
    );
}

export default Checkout;
