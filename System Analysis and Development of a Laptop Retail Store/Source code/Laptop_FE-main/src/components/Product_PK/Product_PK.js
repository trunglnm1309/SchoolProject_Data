import React from "react";
import bannerPK from "../../assets/img/bannerphukien.png";
import { Link } from "react-router-dom";
import transferPrice from "../../helper/TransferPrice";

function Product_PK_Item(props) {
  const { _id, image, title, oldprice, newprice } = props.data;
  return (
    <div class="col-lg-3 p-0">
      <Link
        to={`/DetailProduct/${_id}`}
        style={{ display: "block", textDecoration: "none" }}
        class="product-pk-item"
      >
        <img src={image} alt="" class="product-pk-img" />
        <div class="product-pk-item-desc">
          <p class="product-pk-item-desc-title">{title}</p>
          <div class="product-pk-item-desc-price">
            <span class="product-pk-item-desc-price-old">
              <span>{transferPrice(oldprice)}đ</span>{" "}
              <span class="sale">
                {parseInt((1 - newprice / oldprice) * 100)}%
              </span>
            </span>
            <span class="product-pk-item-desc-price-new">
              {transferPrice(newprice)}đ
            </span>
          </div>
        </div>
      </Link>
    </div>
  );
}
function Product_PK(props) {
  const { ListProduct } = props;
  function render(ListProduct) {
    var content = [];
    if (ListProduct) {
      content = ListProduct.map((item, stt) => (
        <Product_PK_Item data={item} key={stt} />
      ));
    }
    return content;
  }
  return (
    <div class="product-pk">
      <div class="container">
        <div class="product-pk-wrap">
          <img src={bannerPK} alt="" class="product-pk-bg" />
          <div class="row m-0">{render(ListProduct)}</div>
          <div class="product-pk-link">
            <Link to="/Products/PK" class="product-pk-btn">
              Xem Tiếp
            </Link>
          </div>
        </div>
      </div>
    </div>
  );
}

export default Product_PK;
