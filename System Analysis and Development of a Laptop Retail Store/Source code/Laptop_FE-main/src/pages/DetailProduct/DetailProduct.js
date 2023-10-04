import React, { useEffect, useState } from "react";
import Breadcum from "../../components/Breadcum/Breadcum";
import CallApi from "../../helper/axiosClient";
import "./DetailProduct.css";
import DetailProductComponent from "../../components/DetailProduct/DetailProductComponent";
import SlideProduct from "../../components/Slider/slideproduct";
import ConfigProduct from "../../components/DetailProduct/ConfigProduct";
import Comment from "../../components/DetailProduct/Comment";
import parse from "html-react-parser";
import Loader from "react-loader-spinner";
import { Link } from "react-router-dom";

function DetailProduct(props) {
    const { id } = props.match.params;
    const [MoreContent, setMoreContent] = useState(false);
    const [Product, setProduct] = useState(null);
    const [ProductRecommend, setProductRecommed] = useState(null);
    const [ProductPk, setProductPk] = useState(null);
    const [Loading, SetLoading] = useState(true);
    useEffect(() => {
        SetLoading(true);
        window.scrollTo(0, 0);
        async function getProduct() {
            // SetLoading(true);
            var dataProductPromise = CallApi({
                url: `/api/detailproducts/${id}`,
                method: "get",
            });
            var dataProductRecommendPromise = CallApi({
                url: `/api/products/Laptop?type=1`,
                method: "get",
            });
            var dataProductPkPromise = CallApi({
                url: `/api/products/PK`,
                method: "get",
            });
            const dataProduct = await dataProductPromise;
            const dataProductPk = await dataProductRecommendPromise;
            const dataProductRecommend = await dataProductPkPromise;
            setProduct(dataProduct);
            setProductPk(dataProductPk);
            setProductRecommed(dataProductRecommend);
            SetLoading(false);
        }
        getProduct();
    }, [id]);
    function onShowMoreContent() {
        setMoreContent(true);
    }
    function renderNotify(notify) {}
    return Loading ? (
        <div
            style={{
                padding: "30px 0",
            }}
        >
            <Loader
                type="Circles"
                color="#f50057"
                height={100}
                width={100}
                style={{ textAlign: "center", width: "100%" }}
            />
        </div>
    ) : (
        <div className="DetailProduct">
            <div className="container">
                <Breadcum
                    ListBreadcum={[Product.category]}
                    final={Product.title}
                />
                <div className="row">
                    <DetailProductComponent
                        Product={Product}
                        Notify={renderNotify}
                    />
                    <div class="col-lg-3 css-80">
                        <div class="css-100">
                            <div class="css-81 css-1002">
                                <i class="fas fa-truck .css-82"></i>
                                <span class="css-83">
                                    Sản phẩm được miễn phí giao hàng{" "}
                                </span>
                            </div>
                            <div class="css-84">
                                <h1 class="css-85">Chính sách bán hàng</h1>
                                <ul class="css-86">
                                    <li class="css-87">
                                        <i class="fas fa-check css-88"></i>
                                        Cam kết hàng chính hãng 100%
                                    </li>
                                    <li class="css-87">
                                        <i class="fas fa-truck css-88"></i>
                                        Miễn phí giao hàng từ 500K
                                    </li>
                                    <li class="css-87">
                                        <i class="fas fa-undo-alt css-88"></i>
                                        Đổi trả miễn phí trong 10 ngày
                                    </li>
                                </ul>
                            </div>
                            <div class="css-84">
                                <h1 class="css-85">Dịch vụ khác</h1>
                                <ul class="css-86">
                                    <li class="css-87">
                                        <i class="fas fa-cog css-88"></i>
                                        Sửa chữa đồng giá 150.000đ.
                                    </li>
                                    <li class="css-87">
                                        <i class="fas fa-laptop css-88"></i>
                                        Vệ sinh máy tính, laptop.
                                    </li>
                                    <li class="css-87">
                                        <i class="fas fa-shield-alt css-88"></i>
                                        Bảo hành tại nhà.
                                    </li>
                                </ul>
                            </div>
                        </div>
                    </div>
                </div>

                <div className="row">
                    <div className="home col-lg-12">
                        <div class="header-part-2 ">
                            <div class="header-text">
                                <h1 class="header-title-1">
                                    Sản phẩm tương tự
                                </h1>
                                <Link to={"/Products/Laptop"} class="header-a">
                                    Xem tất cả
                                </Link>
                            </div>
                        </div>
                        {ProductRecommend && (
                            <SlideProduct
                                ListProduct={ProductRecommend.listProducts}
                            />
                        )}
                    </div>
                </div>
                <div className="row">
                    <div className="home col-lg-12">
                        <div class="header-part-2 ">
                            <div class="header-text">
                                <h1 class="header-title-1">Sản phẩm đi kèm</h1>
                                <Link to={"/Products/PK"} class="header-a">
                                    Xem tất cả
                                </Link>
                            </div>
                        </div>
                        {ProductPk && (
                            <SlideProduct
                                ListProduct={ProductPk.listProducts}
                            />
                        )}
                    </div>
                </div>

                <div className="home2">
                    <div
                        className="product-info "
                        style={MoreContent ? { height: "inherit" } : {}}
                    >
                        <div className="pr-5 product-description col-lg-8">
                            <h1 class="header-h1-1">Mô tả sản phẩm</h1>
                            <div className="product-description-wrap">
                                {parse(Product.content)}
                            </div>
                        </div>
                        <ConfigProduct />
                        {!MoreContent && (
                            <div class="product-btn-1">
                                <span onClick={onShowMoreContent}>
                                    Xem thêm nội dung
                                </span>{" "}
                                <i class="fas fa-chevron-down"></i>
                            </div>
                        )}
                    </div>
                </div>
                <div className="row">
                    <div className="col-lg-12">
                        <Comment id={id} />
                    </div>
                </div>
            </div>
        </div>
    );
}

export default DetailProduct;
