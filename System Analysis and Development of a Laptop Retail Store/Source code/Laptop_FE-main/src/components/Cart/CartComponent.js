import React, { useState } from "react";
import CartEmpty from "./CartEmpty";
import { confirmAlert } from "react-confirm-alert";
import "react-confirm-alert/src/react-confirm-alert.css";
import { useSelector, useDispatch } from "react-redux";
import * as actions from "./../../actions/index";
import { toast } from "react-toastify";
import { useHistory } from "react-router-dom";
import transferPrice from "../../helper/TransferPrice";
function CartItemComponent(props) {
    const { data } = props;
    const dispatch = useDispatch();
    return (
        <div class="cart__item">
            <div class="cart__item-info">
                <img src={data.image} alt="" class="cart__item-img" />
                <div class="cart__item-info-list">
                    <span class="cart__item-name">{data.title}</span>
                    <span class="cart__item-name-qr">{data.id}</span>
                </div>
            </div>
            <div class="cart__item-detail">
                <div>
                    <button
                        className="cart__item-remove"
                        onClick={() =>
                            dispatch(actions.changeQuantityCart(-1, data))
                        }
                    >
                        <i
                            class={`${
                                data.soluong === 1
                                    ? "far fa-trash-alt"
                                    : "fas fa-minus"
                            }`}
                        ></i>
                    </button>
                    <span class="cart__item-quantity">{data.soluong}</span>
                    <button
                        class="cart__item-add"
                        onClick={() =>
                            dispatch(actions.changeQuantityCart(1, data))
                        }
                    >
                        <i class="fas fa-plus"></i>
                    </button>
                </div>
            </div>
            <div class="cart__item-price">
                <div class="cart__item-price-current">
                    {transferPrice(data.newprice * data.soluong)}đ
                </div>
                <div class="cart__item-price-old">
                    {transferPrice(data.oldprice * data.soluong)}đ
                </div>
            </div>
        </div>
    );
}

function CartComponent(props) {
    const ListCart = useSelector((state) => state.cart);
    const dispatch = useDispatch();
    const user = useSelector((state) => state.user);
    const history = useHistory();
    function renderTotalMoney() {
        var total = 0;
        if (ListCart.length > 0) {
            ListCart.forEach((item) => {
                total += item.soluong * item.newprice;
            });
        }
        return total;
    }
    function handleDeleteAllItem() {
        if (ListCart && ListCart.length > 0) {
            confirmAlert({
                title: "Cảnh báo",
                message: "Bạn có chắn muốn xóa tất cả giỏ hàng",
                buttons: [
                    {
                        label: "Yes",
                        onClick: () => {
                            dispatch(actions.deleteAllCart());
                            toast.error("Xóa tất cả sản phẩm thành công");
                        },
                    },
                ],
            });
        }
    }
    function renderListCart() {
        var content = [];
        if (ListCart && ListCart.length > 0)
            content = ListCart.map((item, stt) => (
                <CartItemComponent data={item} key={stt} />
            ));
        else content = <CartEmpty></CartEmpty>;
        return content;
    }
    function checkQuantity(ListCart) {
        var temp = [];
        if (ListCart && ListCart.length > 0) {
            temp = ListCart.filter(
                (item, stt) => item.quantity - item.soluong < 0
            );
        }
        return temp;
    }
    function onClickCheckOut() {
        if (!user) {
            toast.error("Vui lòng đăng nhập để thanh toán");
            return;
        } else if (checkQuantity(ListCart).length > 0) {
            var ProductNotQuantiy = checkQuantity(ListCart);
            ProductNotQuantiy.forEach((item) => {
                if (item.quantity < 1) {
                    toast.error(
                        `Sản phẩm ${item.title} đã hết hàng vui lòng quay lại sau`
                    );
                } else {
                    toast.error(
                        `Sản phẩm ${item.title} chỉ còn lại ${item.quantity} sản phẩm vui lòng chọn lại số lượng`
                    );
                }
            });
            return;
        }
        history.push("/checkout");
    }
    return (
        <div>
            <div class="grid__row">
                <div class="grid__column-8">
                    <div class="cart__info">
                        <span class="cart__info-quantity">
                            Giỏ hàng có {ListCart && ListCart.length} sản phẩm
                        </span>
                        <span
                            class={`${
                                !ListCart || ListCart.length < 1
                                    ? "cart__info-delete--disable"
                                    : "cart__info-delete"
                            }`}
                            onClick={handleDeleteAllItem}
                        >
                            Xóa tất cả
                        </span>
                    </div>
                </div>
            </div>
            <div class="grid__row grid__row-1">
                <div class="grid__column-8">{renderListCart()}</div>
                <div class="grid__column-4">
                    <div class="cart__cupon">
                        <span class="cart__cupon-qr">
                            Mã giảm giá / Phiếu mua hàng
                        </span>
                        <div class="cart__cupon-type">
                            <input
                                type="text"
                                class="cart__cupon-type-input"
                                placeholder="Nhập mã của bạn"
                            />
                            <button class="cart__cupon-type-btn">
                                Áp dụng ngay
                            </button>
                        </div>
                    </div>
                    <div class="cart-pay">
                        <span class="cart-pay-text">
                            Thanh toán(Đã bao gồm VAT)
                        </span>
                        <div class="cart-pay-sum">
                            <span class="cart-pay-sum-a">Tạm tính</span>
                            <span class="cart-pay-sum-money">
                                {transferPrice(renderTotalMoney())}đ
                            </span>
                        </div>
                        <div class="cart-pay-transport">
                            <span class="cart-pay-transport-a">
                                Phí vận chuyển
                            </span>
                            <span class="cart-pay-transport-money">
                                30.000 đ
                            </span>
                        </div>
                        <div class="cart-pay-sale">
                            <span class="cart-pay-sale-a">Khuyến mãi</span>
                            <span class="cart-pay-sale-money">0 đ</span>
                        </div>
                        <div class="cart-pay-check">
                            <span class="cart-pay-check-a">Thành tiền</span>
                            <span class="cart-pay-check-money">
                                {transferPrice(renderTotalMoney() + 30000)}
                            </span>
                        </div>
                        <button
                            class="cart-pay-btn-pay btn-disable"
                            onClick={onClickCheckOut}
                        >
                            TIẾN HÀNH THANH TOÁN
                            {!user && (
                                <div class="cart-pay-btn-pay__title">
                                    Bạn cần đăng nhập để tiếp tục
                                </div>
                            )}
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}

export default CartComponent;
