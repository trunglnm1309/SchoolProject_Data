import React, { useEffect, useLayoutEffect } from "react";
import { Switch, Route, useRouteMatch, Link } from "react-router-dom";
import "./admin.css";
import { useSelector, useDispatch } from "react-redux";
import { toast, ToastContainer } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";
import DashBoard from "../../components/Admin/DashBoard";
import AdminOrder from "../../components/Admin/AdminOrder";
import AdminProduct from "../../components/Admin/AdminProduct";
import AdminUser from "../../components/Admin/AdminUser";

function Admin(props) {
    const dispatch = useDispatch();
    const User = useSelector((state) => state.user);
    const { push } = props.history;
    console.log("Admin");
    let { path } = useRouteMatch();
    useLayoutEffect(() => {
        if (!User) {
            toast.error("Bạn không có quyền truy cập trang quản trị");
            push("/");
        }
    }, []);
    return (
        <div className="admin">
            <div className="admin-sidebar">
                <div className="admin-sidebar-logo">
                    <img src="" alt="" className="admin-sidebar-login-img" />
                </div>
                <div className="admin-sidebar-nav">
                    <Link
                        to={`${path}/notification`}
                        className="admin-sidear-nav-item"
                    >
                        <i class="fas fa-bell"></i>
                        <span>BẢNG ĐIỀU KHIỂN</span>
                    </Link>
                    <Link
                        to={`${path}/product`}
                        className="admin-sidear-nav-item"
                    >
                        <i class="fas fa-bell"></i>
                        <span>SẢN PHẨM</span>
                    </Link>
                    <Link to={`${path}/user`} className="admin-sidear-nav-item">
                        <i class="fas fa-bell"></i>
                        <span>NGƯỜI DÙNG</span>
                    </Link>
                    <Link
                        to={`${path}/order`}
                        className="admin-sidear-nav-item"
                    >
                        <i class="fas fa-bell"></i>
                        <span>ĐƠN HÀNG</span>
                    </Link>
                    <Link to={`/`} className="admin-sidear-nav-item">
                        <i class="fas fa-bell"></i>
                        <span>HOME</span>
                    </Link>
                </div>
            </div>
            <div className="admin-main">
                <Switch>
                    <Route
                        path={`${path}/notification`}
                        component={DashBoard}
                    />
                    <Route path={`${path}/product`} component={AdminProduct} />
                    <Route path={`${path}/user`} component={AdminUser} />
                    <Route path={`${path}/order`} component={AdminOrder} />
                </Switch>
            </div>
            <ToastContainer autoClose={2000} position="bottom-right" />
        </div>
    );
}

export default Admin;
