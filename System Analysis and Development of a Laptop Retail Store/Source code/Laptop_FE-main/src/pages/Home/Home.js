import React, { useEffect, useState } from "react";
import Slider from "../../components/Slider/slider";
import { Link } from "react-router-dom";
import "./home.css";
import Product_PK from "../../components/Product_PK/Product_PK";
import Banner from "../../components/Banner/Banner";
import ImageBanner1 from "../../assets/img/xxxbanner1.webp";
import ImageBanner2 from "../../assets/img/xxxbanner2.webp";
import ImageBanner3 from "../../assets/img/xxxbanner3.webp";
import ImageBanner4 from "../../assets/img/xxxbanner4.webp";
import ImageBanner5 from "../../assets/img/banner2.webp";
import ImageBanner6 from "../../assets/img/banner3.webp";
import ImageBanner7 from "../../assets/img/banner4.webp";
import ImageBanner8 from "../../assets/img/banner5.webp";
import ImageBanner9 from "../../assets/img/banner6.webp";
import ImageBanner10 from "../../assets/img/banner7.webp";
import ImageBanner11 from "../../assets/img/banner8.webp";
import ImageBanner12 from "../../assets/img/banner9.webp";
import SlideProduct from "../../components/Slider/slideproduct";
import axiosClient from "../../helper/axiosClient";
import Loader from "react-loader-spinner";

const Home = () => {
    const [ListProductKM, setListProductKM] = useState(null);
    const [ListProductHot, setListProductHot] = useState(null);
    const [ListProductBC, setListProductBC] = useState(null);
    const [ListProductPK, setListProductPK] = useState(null);
    const [isLoading, setIsLoading] = useState(true);
    useEffect(() => {
        async function getListProduct(category, page, type) {
            return axiosClient({
                url: `/api/products/${category}?page=${page}&type=${type}`,
                method: "get",
            });
        }
        const listPromise = [];
        listPromise.push(
            getListProduct("Laptop", 1, 1),
            getListProduct("Laptop", 1, 2),
            getListProduct("Laptop", 1, 3),
            getListProduct("PK", 1, 1)
        );
        Promise.all(listPromise)
            .then((data) => {
                console.log(
                    "🚀 ~ file: Home.js ~ line 45 ~ .then ~ data",
                    data
                );
                setListProductKM(data[0].listProducts);
                setListProductHot(data[1].listProducts);
                setListProductBC(data[2].listProducts);
                setListProductPK(data[3].listProducts);
                setIsLoading(false);
            })
            .catch(() => setIsLoading(false));
    }, []);
    if (isLoading)
        return (
            <Loader
                type="Circles"
                color="#f50057"
                height={100}
                width={100}
                style={{
                    padding: "30px 0",
                    textAlign: "center",
                    width: "100%",
                }}
            />
        );
    return (
        <div className="main">
            <div className="slide-home">
                <div className="container-fluid">
                    <div className="row">
                        <div className="p-0 col-12">
                            <Slider></Slider>
                        </div>
                    </div>
                </div>
            </div>
            <Banner
                listBanner={[
                    ImageBanner1,
                    ImageBanner2,
                    ImageBanner3,
                    ImageBanner4,
                ]}
            ></Banner>
            <div className="product-deal-hot">
                <div class="container">
                    <div class="product-deal-hot-wrap">
                        <div class="product-hot-intro">
                            <h3 class="product-deal-hot-title">
                                Laptop Khuyến Mãi Trong Tháng
                            </h3>
                            <Link
                                to="/Products/Laptop?type=2"
                                class="product-hot-btn"
                            >
                                Xem tất cả <i class="fas fa-chevron-right"></i>
                            </Link>
                        </div>
                        <div class="row mx-4">
                            <div className="col-12">
                                <SlideProduct
                                    ListProduct={ListProductKM}
                                ></SlideProduct>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <Banner listBanner={[ImageBanner5]} />
            <div className="product-deal-hot">
                <div class="container">
                    <div class="product-deal-hot-wrap">
                        <div class="product-hot-intro">
                            <h3 class="product-deal-hot-title">
                                Laptop Bán Chạy Trong Tháng
                            </h3>
                            <Link
                                to="/Products/Laptop?type=3"
                                class="product-hot-btn"
                            >
                                Xem tất cả <i class="fas fa-chevron-right"></i>
                            </Link>
                        </div>
                        <div class="row mx-4">
                            <div className="col-12">
                                <SlideProduct
                                    ListProduct={ListProductBC}
                                ></SlideProduct>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <Banner listBanner={[ImageBanner6, ImageBanner7, ImageBanner8]} />
            <div className="product-deal-hot">
                <div class="container">
                    <div class="product-deal-hot-wrap">
                        <div class="product-hot-intro">
                            <h3 class="product-deal-hot-title">
                                Laptop Mới Nhất Trong Tháng
                            </h3>
                            <Link to="/Products/Laptop" class="product-hot-btn">
                                Xem tất cả <i class="fas fa-chevron-right"></i>
                            </Link>
                        </div>
                        <div class="row mx-4">
                            <div className="col-12">
                                <SlideProduct
                                    ListProduct={ListProductHot}
                                ></SlideProduct>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div class="product-brand">
                <div class="container">
                    <div class="product-brand-wrap">
                        <h3 class="product-brand-title">Thương hiệu nổi bật</h3>
                        <div class="row px-4 mt-2">
                            <div class="col-lg-3 p-0">
                                <div class="product-brand-item">
                                    <div class="brand-img-wrap">
                                        <img
                                            src={ImageBanner9}
                                            alt=""
                                            class="product-brand-img"
                                        />
                                    </div>
                                    <div class="product-band-desc">
                                        <p class="product-brand-name">MSI</p>
                                        <p class="product-brand-content">
                                            Made for Gamers and Creator
                                        </p>
                                    </div>
                                </div>
                            </div>
                            <div class="col-lg-3 p-0">
                                <div class="product-brand-item">
                                    <div class="brand-img-wrap">
                                        <img
                                            src={ImageBanner10}
                                            alt=""
                                            class="product-brand-img"
                                        />
                                    </div>
                                    <div class="product-band-desc">
                                        <p class="product-brand-name">HP</p>
                                        <p class="product-brand-content">
                                            Lễ hội máy tính HP - Ưu đãi cực phê
                                        </p>
                                    </div>
                                </div>
                            </div>
                            <div class="col-lg-3 p-0">
                                <div class="product-brand-item">
                                    <div class="brand-img-wrap">
                                        <img
                                            src={ImageBanner11}
                                            alt=""
                                            class="product-brand-img"
                                        />
                                    </div>{" "}
                                    <div class="product-band-desc">
                                        <p class="product-brand-name">LG</p>
                                        <p class="product-brand-content">
                                            Tháng LG quà mê ly
                                        </p>
                                    </div>
                                </div>
                            </div>
                            <div class="col-lg-3 p-0">
                                <div class="product-brand-item">
                                    <div class="brand-img-wrap">
                                        <img
                                            src={ImageBanner12}
                                            alt=""
                                            class="product-brand-img"
                                        />
                                    </div>{" "}
                                    <div class="product-band-desc">
                                        <p class="product-brand-name">Lenovo</p>
                                        <p class="product-brand-content">
                                            Laptop chơi game thực thụ
                                        </p>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <Product_PK ListProduct={ListProductPK} />
        </div>
    );
};

export default Home;
