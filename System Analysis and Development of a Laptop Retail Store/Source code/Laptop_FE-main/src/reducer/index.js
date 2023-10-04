import { combineReducers } from "redux";
import modalLogin from "./modalLogin";
import cart from "./cart";
import user from "./user";
import siderbarOrder from "./sidebarOrder";
import order from "./order";
import reply from "./reply";
import adminProduct from "./AdminProduct";
const reducer = combineReducers({
    modalLogin,
    cart,
    user,
    order,
    reply,
    siderbarOrder,
    adminProduct,
});

export default reducer;
