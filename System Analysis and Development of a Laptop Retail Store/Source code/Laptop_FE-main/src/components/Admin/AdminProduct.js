import React, { useState } from "react";
import { useDispatch } from "react-redux";
import axiosClient from "../../helper/axiosClient";
import { toast } from "react-toastify";
import { useEffect } from "react";
import Loader from "react-loader-spinner";
import { useSelector } from "react-redux";
import * as action from "../../actions/productAdmin";
import { useCallback } from "react";
import Quill from "quill";
function AddProduct(props) {
    const { RenderListProduct } = props;
    const { data } = useSelector((state) => state.adminProduct);
    const [quill, setquill] = useState(null);
    const dispatch = useDispatch();
    const TOOLBAR_OPTIONS = [
        [{ header: [1, 2, 3, 4, 5, 6, false] }],
        [{ font: [], fontsize: [] }],
        [{ list: "ordered" }, { list: "bullet" }],
        ["bold", "italic", "underline"],
        [{ color: [] }, { background: [] }],
        [{ script: "sub" }, { script: "super" }],
        [{ align: [] }],
        ["image", "blockquote", "code-block", "link"],
        [{ size: ["small", false, "large", "huge"] }],
        [{ indent: "-1" }, { indent: "+1" }],
        ["clean"],
    ];
    const wrapperRef = useCallback((wrapper) => {
        if (wrapper === null) {
            return;
        }
        wrapper.innerHTML = "";
        const editor = document.createElement("div");
        wrapper.append(editor);
        const q = new Quill(editor, {
            theme: "snow",
            modules: { toolbar: TOOLBAR_OPTIONS },
        });
        setquill(q);
    }, []);
    const [FormValue, setFormValue] = useState({
        title: data ? data.title : "",
        newprice: data ? data.newprice : "",
        oldprice: data ? data.oldprice : "",
        image: [],
        listimage: {},
        type: data ? data.type : "",
        category: data ? data.category : "",
        content: data ? data.content : "",
        quantity: data ? data.quantity : "",
    });
    function onSubmitForm(e) {
        e.preventDefault();
        var formData = new FormData();
        for (const x in FormValue.listimage) {
            formData.append("listimage", FormValue.listimage[x]);
        }
        formData.append("quantity", FormValue.quantity);
        formData.append("image", FormValue.image);
        formData.append("type", FormValue.type);
        formData.append("category", FormValue.category);
        formData.append("content", quill.root.innerHTML);
        formData.append("title", FormValue.title);
        formData.append("newprice", FormValue.newprice);
        formData.append("oldprice", FormValue.oldprice);
        if (data && data._id) {
            formData.append("id", data._id);
            axiosClient({
                url: "/api/product",
                method: "put",
                data: formData,
            }).then((data) => {
                if (data) {
                    toast.success("Cập nhật sản phẩm thành công");
                    dispatch(action.CloseFormProduct());
                    RenderListProduct();
                }
            });
        } else {
            axiosClient({
                url: "/api/product",
                method: "post",
                data: formData,
            }).then((data) => {
                if (data) {
                    toast.success("Thêm sản phẩm thành công");
                    dispatch(action.CloseFormProduct());

                    RenderListProduct();
                }
            });
        }
    }
    function handleChange(e) {
        if (e.target.name === "image") {
            setFormValue({
                ...FormValue,
                [e.target.name]: e.target.files[0],
            });
        } else if (e.target.name === "listimage") {
            setFormValue({
                ...FormValue,
                [e.target.name]: e.target.files,
            });
        } else {
            setFormValue({
                ...FormValue,
                [e.target.name]: e.target.value,
            });
        }
    }

    function onCloseFormAdd() {
        dispatch(action.CloseFormProduct());
    }
    return (
        <form onSubmit={onSubmitForm}>
            <div class="form-group">
                <label for="exampleFormControlInput1">Tên sản phẩm</label>
                <input
                    type="text"
                    class="form-control"
                    value={FormValue.title}
                    onChange={handleChange}
                    name="title"
                    id="exampleFormControlInput1"
                    placeholder="Laptop MSI Modern 14 A10RB-888VN (14 FHD/i7-10510U/8GB/512GB SSD/GeForce MX250/Win10/1.2kg)"
                />
            </div>
            <div class="form-group">
                <label for="exampleFormControlInput2">Giá cũ</label>
                <input
                    type="number"
                    class="form-control"
                    value={FormValue.oldprice}
                    onChange={handleChange}
                    name="oldprice"
                    id="exampleFormControlInput2"
                    placeholder="10"
                />
            </div>
            <div class="form-group">
                <label for="exampleFormControlInput2">Giá mới</label>
                <input
                    type="number"
                    class="form-control"
                    value={FormValue.newprice}
                    onChange={handleChange}
                    name="newprice"
                    id="exampleFormControlInput2"
                    placeholder="10"
                />
            </div>
            <div class="form-group">
                <label for="exampleFormControlInput2">Số lượng</label>
                <input
                    type="number"
                    class="form-control"
                    value={FormValue.quantity}
                    onChange={handleChange}
                    name="quantity"
                    id="exampleFormControlInput2"
                    placeholder="10"
                />
            </div>
            <div class="form-group">
                <label for="exampleFormControlTextarea1">Mô tả sản phẩm</label>
                <div ref={wrapperRef}></div>
            </div>
            <div class="form-group">
                <label for="exampleFormControlSelect3">Thế loại</label>
                <select
                    class="form-control"
                    id="exampleFormControlSelect3"
                    value={FormValue.category}
                    onChange={handleChange}
                    name="category"
                >
                    <option value="">Chọn thế loại</option>
                    <option value="Laptop">Laptop</option>
                    <option value="Màn Hình">Màn Hình</option>
                    <option value="Chuột">Chuột</option>
                    <option value="Bàn Phím">Bàn Phím</option>
                    <option value="PC">PC</option>
                </select>
            </div>
            <div class="form-group">
                <label for="exampleFormControlSelect3">Thế loại</label>
                <select
                    class="form-control"
                    id="exampleFormControlSelect3"
                    value={FormValue.type}
                    onChange={handleChange}
                    name="type"
                >
                    <option value="">Chọn loại sản phẩm</option>
                    <option value="1">Mới nhất</option>
                    <option value="2">Bán chạy</option>
                    <option value="3">Giảm giá</option>
                </select>
            </div>
            <div class="form-group">
                <label for="exampleFormControlFile4">Hình ảnh</label>
                <input
                    type="file"
                    name="image"
                    accept="image/*"
                    onChange={handleChange}
                    class="form-control-file"
                    id="exampleFormControlFile4"
                />
            </div>
            <div class="form-group">
                <label for="exampleFormControlFile4">Hình ảnh</label>
                <input
                    type="file"
                    name="listimage"
                    accept="image/*"
                    multiple
                    onChange={handleChange}
                    class="form-control-file"
                    id="exampleFormControlFile4"
                />
            </div>
            <div style={{ textAlign: "center" }}>
                <button type="submit" class="btn btn-primary mb-2">
                    {data ? "Sửa" : "Thêm"}
                </button>
                <button
                    class="btn btn-secondary mb-2 ml-5"
                    onClick={onCloseFormAdd}
                >
                    Hủy
                </button>
            </div>
        </form>
    );
}
function AdminProductItem(props) {
    const dispatch = useDispatch();
    const { title, newprice, quantity, oldprice, image, type, category, _id } =
        props.data;
    const { RenderListProduct } = props;
    function onUpdateProduct() {
        dispatch(action.UpdateProduct(props.data));
    }
    function onDeleteProduct() {
        axiosClient({
            url: "/api/product",
            method: "DELETE",
            data: {
                id: _id,
            },
        }).then((data) => {
            if (data === "Thành công") {
                RenderListProduct();
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
            <td class="product-name">{title}</td>
            <td class="product-price">{oldprice}</td>
            <td class="product-price">{newprice}</td>
            <td class="product-stock-status">{quantity}</td>
            <td class="product-price">{category}</td>
            <td class="product-price">{type}</td>
            <td class="product-add-to-cart">
                <a href="#" class="btn btn-primary" onClick={onUpdateProduct}>
                    Sửa
                </a>
            </td>
            <td class="product-remove" data-title="Remove">
                <a href="#" class="btn btn-danger" onClick={onDeleteProduct}>
                    {" "}
                    Xóa
                </a>
            </td>
        </tr>
    );
}
function AdminProduct() {
    const { isOpenForm } = useSelector((state) => state.adminProduct);
    const dispatch = useDispatch();
    const [ListProduct, setListProduct] = useState({
        data: [],
        loading: false,
    });
    const [isRenderListProduct, setisRenderListProduct] = useState(false);
    useEffect(() => {
        setListProduct({ ...ListProduct, loading: true });
        axiosClient({
            url: "/api/admin/products",
            method: "get",
        }).then((data) => {
            setListProduct({
                data: data,
                loading: false,
            });
        });
    }, [isRenderListProduct]);
    function RenderListProduct() {
        setisRenderListProduct(!isRenderListProduct);
    }
    function ShowProduct(arr) {
        let content = [];
        if (arr) {
            content = arr.map((data, stt) => (
                <AdminProductItem
                    key={stt}
                    data={data}
                    RenderListProduct={RenderListProduct}
                />
            ));
        }
        return content;
    }
    function onOpenFormAdd() {
        dispatch(action.OpenFormProduct());
    }
    return (
        <div className="AdminProduct">
            <div className="container">
                <h2 className="admin-title">Quản lý sản phẩm</h2>
                <div className="admin-control">
                    <button
                        className="admin-controll-add btn btn-primary"
                        onClick={onOpenFormAdd}
                    >
                        Thêm sản phẩm
                    </button>
                </div>
                <div className="AdminProduct-list">
                    <div class="row">
                        <div class="col-12">
                            {isOpenForm ? (
                                <AddProduct
                                    RenderListProduct={RenderListProduct}
                                />
                            ) : (
                                <div class="table-responsive wishlist_table">
                                    <table class="table">
                                        <thead>
                                            <tr>
                                                <th class="product-thumbnail">
                                                    Hình Ảnh
                                                </th>
                                                <th class="product-name">
                                                    Title
                                                </th>
                                                <th class="product-price">
                                                    Price old
                                                </th>
                                                <th class="product-price">
                                                    Price new
                                                </th>
                                                <th class="product-stock-status">
                                                    quantity
                                                </th>
                                                <th class="product-price">
                                                    category
                                                </th>
                                                <th class="product-price">
                                                    type
                                                </th>
                                                <th class="product-add-to-cart">
                                                    Sửa
                                                </th>
                                                <th class="product-remove">
                                                    Remove
                                                </th>
                                            </tr>
                                        </thead>
                                        <tbody>
                                            {ListProduct.loading ? (
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
                                                ShowProduct(ListProduct.data)
                                            )}
                                        </tbody>
                                    </table>
                                </div>
                            )}
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}
export default AdminProduct;
