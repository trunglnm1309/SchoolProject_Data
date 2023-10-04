import React, { useState } from "react";
import { useEffect } from "react";
import AxiosClient from "../../helper/axiosClient";
import Loader from "react-loader-spinner";
import { toast } from "react-toastify";

function AdminUserItem(props) {
    const { username, email, role, image, enable, _id } = props.data;
    const { isReRender } = props;
    function onUpadatedAdmin() {}
    function onDeleteUser() {
        AxiosClient({
            url: "/api/auth/user",
            method: "DELETE",
            data: {
                id: _id,
            },
        }).then((data) => {
            if (data === "Thành công") {
                isReRender();
                toast.success("Xóa Thành Công");
            } else {
                toast.error("Có lỗi trong quá trình xóa vui lòng thử lại");
            }
        });
    }
    return (
        <tr>
            <td class="product-thumbnail">
                <img src={image} alt="product1" />
            </td>
            <td class="product-name" data-title="Product">
                {username}
            </td>
            <td class="product-name" data-title="Product">
                {email}
            </td>
            <td class="product-name" data-title="Product">
                {enable ? (
                    <p class="text-success">Đã kích hoạt</p>
                ) : (
                    <p class="text-danger">Chưa kích hoạt</p>
                )}
            </td>
            <td class="product-name" data-title="Product">
                {role}
            </td>
            {role != "ADMIN" ? (
                <td class="product-add-to-cart">
                    <button class="btn btn-primary" onClick={onUpadatedAdmin}>
                        Cấp quyền Admin
                    </button>
                </td>
            ) : (
                <td></td>
            )}
            <td class="product-remove" data-title="Remove">
                <button class="btn btn-danger" onClick={onDeleteUser}>
                    {" "}
                    Xóa
                </button>
            </td>
        </tr>
    );
}

function AdminUser() {
    const [isReRender, setisReRender] = useState(false);
    const [ListUser, setListUser] = useState({
        data: null,
        isLoading: false,
    });

    useEffect(() => {
        setListUser({
            ...ListUser,
            isLoading: true,
        });
        AxiosClient({
            url: "/api/auth/users",
            method: "get",
        }).then((data) => {
            setListUser({
                data: data,
                isLoading: false,
            });
        });
    }, [isReRender]);
    function render() {
        setisReRender(!isReRender);
    }
    function renderUserItem(arr) {
        let content = [];
        if (arr) {
            content = arr.map((data, stt) => (
                <AdminUserItem key={stt} data={data} isReRender={render} />
            ));
        }
        return content;
    }
    return (
        <div className="AdminProduct">
            <div className="container">
                <h2 className="admin-title">Quản lý người dùng</h2>

                <div className="AdminProduct-list">
                    <div class="row">
                        <div class="col-12">
                            <div class="table-responsive wishlist_table">
                                <table class="table">
                                    <thead>
                                        <tr>
                                            <th class="product-thumbnail">
                                                Hình Ảnh
                                            </th>
                                            <th class="product-name">
                                                Username
                                            </th>
                                            <th class="product-name">Email</th>
                                            <th class="product-price">
                                                Trạng thái
                                            </th>
                                            <th class="product-price">Quyền</th>
                                            <th class="product-remove">
                                                Cấp quyền
                                            </th>
                                            <th class="product-remove">
                                                Remove
                                            </th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {ListUser.loading ? (
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
                                            renderUserItem(ListUser.data)
                                        )}
                                    </tbody>
                                </table>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}

export default AdminUser;
