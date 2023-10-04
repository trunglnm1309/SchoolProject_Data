import React, { useEffect } from "react";
import transferPrice from "../../helper/TransferPrice";
import { useDispatch } from "react-redux";
import axiosClient from "../../helper/axiosClient";
import { confirmAlert } from "react-confirm-alert";
import OrderItem from "./OrderItem";
import * as action from "../../actions/order";
import "./Order.css";
import { toast } from "react-toastify";

function Order(props) {
    useEffect(() => {
        window.addEventListener("click", (e) => {
            var temp = document.querySelector(".order-detail");
            if (temp === e.target) {
                closeSidebar();
            }
        });
    }, []);
    const dispatch = useDispatch();
    const { ListOrder, isRender } = props;
    function render(data) {
        let temp;
        if (data) {
            temp = data.products.map((item, stt) => (
                <OrderItem key={stt} data={item} />
            ));
        }
        return temp;
    }
    function closeSidebar() {
        dispatch(action.closeOrderDetail());
    }
    function onClickDetailOrder() {
        dispatch(action.addOrder(ListOrder));
        dispatch(action.openOrderDetail());
    }
    function onClickDelete() {
        confirmAlert({
            title: "Cảnh báo",
            message: "Bạn có chắc muốn xóa đơn hàng không?",
            buttons: [
                {
                    label: "Yes",
                    onClick: () => {
                        axiosClient({
                            url: "/api/auth/order",
                            method: "delete",
                            data: ListOrder,
                        }).then((data) => {
                            if (data.statusCode === 200) {
                                isRender();
                                toast.success(data.msg);
                            } else {
                                toast.error(data.msg);
                            }
                        });
                    },
                },
                {
                    label: "No",
                },
            ],
        });
    }
    function onClickCancel() {
        confirmAlert({
            title: "Cảnh báo",
            message: "Bạn có chắn muốn hủy đơn hàng không?",
            buttons: [
                {
                    label: "Yes",
                    onClick: () => {
                        axiosClient({
                            url: "/api/auth/order/cancel",
                            method: "post",
                            data: ListOrder,
                        }).then((data) => {
                            if (data.statusCode === 200) {
                                isRender();
                                toast.success(data.msg);
                            } else {
                                toast.error(data.msg);
                            }
                        });
                    },
                },
                {
                    label: "No",
                },
            ],
        });
    }
    function OnClickReOrder() {
        confirmAlert({
            title: "Cảnh báo",
            message: "Bạn có muốn đặt hàng lại đơn hàng này?",
            buttons: [
                {
                    label: "Yes",
                    onClick: () => {
                        axiosClient({
                            url: "/api/auth/payment/ReOrder",
                            method: "post",
                            data: ListOrder,
                        }).then((data) => {
                            if (data.statusCode === 200) {
                                isRender();
                                toast.success(data.msg);
                            } else {
                                toast.error(data.msg);
                            }
                        });
                    },
                },
                {
                    label: "No",
                },
            ],
        });
    }
    console.log(ListOrder.cancelreason);
    return (
        <div className="order-wrap mt-4">
            <div className="order-wrap-item">{render(ListOrder)}</div>
            <div className="order-wrap-control">
                <div className="order-wrap-totalPrice">
                    <span className="order-wrap-totalPrice-title">
                        Tổng Số Tiền
                    </span>
                    <span className="oder-wrap-totalPrice-number">
                        {ListOrder && transferPrice(ListOrder.totalPrice)} đ
                    </span>
                </div>
                <div className="wrap-control-content">
                    <button
                        type="button"
                        class="btn mx-2 btn-danger"
                        onClick={onClickDelete}
                    >
                        Xóa Đơn Hàng
                    </button>
                    {ListOrder && !ListOrder.status_order ? (
                        ListOrder.cancelreason != null ? (
                            ""
                        ) : (
                            <button
                                type="button"
                                class="btn mx-2 btn-danger"
                                onClick={onClickCancel}
                            >
                                Hủy Đơn Hàng
                            </button>
                        )
                    ) : (
                        <button
                            type="button"
                            class="btn mx-2 btn-success"
                            onClick={OnClickReOrder}
                        >
                            Đặt hàng lại
                        </button>
                    )}
                    <button
                        type="button"
                        class={`btn mx-2 ${
                            ListOrder && ListOrder.status_order
                                ? "btn-success"
                                : "btn-danger"
                        }`}
                    >
                        Tình Trạng:{" "}
                        {ListOrder && ListOrder.status_order
                            ? "Đã duyệt"
                            : ListOrder.cancelreason != null
                            ? "Đã hủy"
                            : "Chờ xét duyệt"}
                    </button>
                    <button
                        type="button"
                        class="btn mx-2 btn-primary"
                        onClick={onClickDetailOrder}
                    >
                        Xem chi tiết đơn hàng
                    </button>
                </div>
            </div>
        </div>
    );
}

export default Order;
