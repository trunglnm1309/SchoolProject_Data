import { Tabs } from "antd";
import React, { useEffect, useState } from "react";
import Loader from "react-loader-spinner";
import { useDispatch, useSelector } from "react-redux";
import AxiosInstance from "../../helper/axiosClient";
import transferPrice from "../../helper/TransferPrice";
import OrderItem from "../Order/OrderItem";
import * as action from "../../actions/order";
import { confirmAlert } from "react-confirm-alert";
import { toast } from "react-toastify";
function transferDate(time) {
    var y = new Date(time);
    return `${y.getHours()} Giờ ${y.getMinutes()} Phút, Ngày ${y.getDate()} Tháng ${
        y.getMonth() + 1
    } Năm ${y.getFullYear()}`;
}
function AdminOrderItem(props) {
    const dispatch = useDispatch();
    const { ListOrder, isRender } = props;
    function renderOrderItem(data) {
        let temp;
        if (data) {
            temp = data.products.map((item, stt) => (
                <OrderItem key={stt} data={item} />
            ));
        }

        return temp;
    }
    function onClickDetailOrder() {
        dispatch(action.addOrder(ListOrder));
        dispatch(action.openOrderDetail());
    }
    function onAcceptOrder() {
        confirmAlert({
            title: "Xác nhận",
            message: "Bạn có chắc muốn duyệt đơn hàng không?",
            buttons: [
                {
                    label: "Yes",
                    onClick: () => {
                        AxiosInstance({
                            url: "/api/auth/order/accept",
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
    function onCancleOrder() {
        confirmAlert({
            title: "Cảnh báo",
            message: "Bạn có chắc muốn hủy đơn hàng không?",
            buttons: [
                {
                    label: "Yes",
                    onClick: () => {
                        AxiosInstance({
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
    function renderControll(data) {
        var temp = [];
        if (data.cancelreason) {
            temp = (
                <button type="button" class="btn mx-2 btn-danger">
                    Trạng Thái : Đã Hủy
                </button>
            );
        } else if (data.status_order) {
            temp = (
                <button type="button" class="btn mx-2 btn-success">
                    Trạng Thái : Đã duyệt
                </button>
            );
        } else {
            temp.push(
                <button
                    type="button"
                    class="btn mx-2 btn-danger"
                    onClick={onCancleOrder}
                >
                    Hủy đơn hàng
                </button>
            );
            temp.push(
                <button
                    type="button"
                    class="btn mx-2 btn-success"
                    onClick={onAcceptOrder}
                >
                    Duyệt đơn hàng
                </button>
            );
        }
        return temp;
    }
    return (
        ListOrder && (
            <div className="order-wrap mt-4">
                <div className="order-wrap-item">
                    {renderOrderItem(ListOrder)}
                </div>
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
                        {renderControll(ListOrder)}
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
        )
    );
}

function AdminOrder() {
    const { TabPane } = Tabs;
    const dispatch = useDispatch();
    const sidebar = useSelector((state) => state.siderbarOrder);
    const order = useSelector((state) => state.order);
    const user = useSelector((state) => state.user);
    const [ListOrder, setListOrder] = useState({
        isLoading: false,
        data: [],
    });
    const [render, setrender] = useState(false);
    useEffect(() => {
        window.addEventListener("click", (e) => {
            var temp = document.querySelector(".order-detail");
            if (temp === e.target) {
                closeSidebar();
            }
        });
    }, []);
    useEffect(() => {
        setListOrder({
            ...ListOrder,
            isLoading: true,
        });
        AxiosInstance({
            url: "/api/auth/orders",
            method: "get",
        }).then((data) => {
            setListOrder({
                data: data,
                isLoading: false,
            });
        });
    }, [render]);
    function closeSidebar() {
        dispatch(action.closeOrderDetail());
    }
    function isRender() {
        return setrender(!render);
    }

    function renderOrder(data, key) {
        let temp;
        if (data && data.data != "" && key === 1) {
            temp = data.map((item, stt) => (
                <AdminOrderItem
                    key={stt}
                    ListOrder={item}
                    isRender={isRender}
                />
            ));
        } else if (data && data.data != "" && key === 2) {
            let filter = data.filter(
                (item) => !item.status_order && item.cancelreason === null
            );
            temp = filter.map((item, stt) => (
                <AdminOrderItem
                    key={stt}
                    ListOrder={item}
                    isRender={isRender}
                />
            ));
        } else if (data && data.data != "" && key === 3) {
            let filter = data.filter((item) => item.status_order);
            temp = filter.map((item, stt) => (
                <AdminOrderItem
                    key={stt}
                    ListOrder={item}
                    isRender={isRender}
                />
            ));
        } else if (data && data.data != "" && key === 4) {
            let filter = data.filter((item) => item.cancelreason != null);
            temp = filter.map((item, stt) => (
                <AdminOrderItem
                    key={stt}
                    ListOrder={item}
                    isRender={isRender}
                />
            ));
        }

        return temp;
    }

    return (
        <div>
            <div className="row">
                <div className="col-12">
                    <div className="order order-admin">
                        <h3 className="order-title">
                            Đơn Hàng
                            <span>
                                {ListOrder && ListOrder.data.length} Đơn hàng
                            </span>
                        </h3>
                        <Tabs defaultActiveKey="1">
                            <TabPane tab="Tất cả" key="1">
                                {ListOrder.isLoading ? (
                                    <Loader
                                        type="Circles"
                                        color="#f50057"
                                        height={100}
                                        width={100}
                                        style={{
                                            textAlign: "center",
                                            width: "100%",
                                        }}
                                    />
                                ) : (
                                    renderOrder(ListOrder.data, 1)
                                )}
                            </TabPane>
                            <TabPane tab="Chờ duyệt" key="2">
                                {renderOrder(ListOrder.data, 2)}
                            </TabPane>
                            <TabPane tab="Đã duyệt" key="3">
                                {renderOrder(ListOrder.data, 3)}
                            </TabPane>
                            <TabPane tab="Đã Hủy" key="4">
                                {renderOrder(ListOrder.data, 4)}
                            </TabPane>
                        </Tabs>
                    </div>
                </div>
            </div>
            <div
                className="order-detail"
                style={sidebar ? { height: "100%" } : { height: "0" }}
            >
                <div
                    className="order-detail-wrap"
                    style={
                        sidebar
                            ? { transform: "translateY(0)" }
                            : { transform: "translateY(100%)" }
                    }
                >
                    <div className="order-detail-title">
                        <h3>Chi tiết đơn hàng</h3>
                        <i class="fas fa-times"></i>
                    </div>
                    <div className="order-detail-content">
                        <div className="order-detail-item">
                            <b>Địa chỉ: </b>
                            <span>{order && order.address}</span>
                        </div>
                        <div className="order-detail-item">
                            <b>Số điện thoại: </b>
                            <span
                                style={{ color: "#fe3834", fontWeight: "400" }}
                            >
                                {user && user.phone}
                            </span>
                        </div>
                        <div className="order-detail-item">
                            <b>Thanh toán: </b>
                            <span
                                style={{ color: "#1890ff", fontWeight: "400" }}
                            >
                                {order && order.payment === 1
                                    ? "Thanh Toán Khi Nhận Hàng"
                                    : "Thanh toán qua thẻ"}
                            </span>
                        </div>
                        <div className="order-detail-item">
                            <b>Ngày đặt hàng: </b>
                            <span
                                style={{ color: "#8395a7", fontWeight: "400" }}
                            >
                                {order && transferDate(order.timeorder)}
                            </span>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}

export default AdminOrder;
