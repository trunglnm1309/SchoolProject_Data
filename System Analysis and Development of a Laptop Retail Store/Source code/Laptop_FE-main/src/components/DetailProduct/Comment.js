import React, { useState, useEffect } from "react";
import axiosClient from "../../helper/axiosClient";
import { toast } from "react-toastify";
import { useSelector, useDispatch } from "react-redux";
import * as action from "../../actions/reply";
import { useLocation } from "react-router-dom";
import PaginationComment from "./../Pagination/PaginationComment";
function generateStar(number) {
    var arr = [];
    for (var i = 1; i <= number; i++) {
        arr.push(<i class="color-star fas fa-star star" key={i}></i>);
    }
    for (var i = number + 1; i <= 5; i++) {
        arr.push(<i class="color-star far fa-star" key={i}></i>);
    }
    return arr;
}
function transferDate(time) {
    var y = new Date(time);
    var x = new Date();
    var avengerTime = parseInt(Math.abs(x - y) / 3600000);
    console.log(avengerTime);
    if (avengerTime < 24) {
        if (avengerTime < 1) {
            return Math.abs(x.getMinutes() - y.getMinutes()) + " Phút trước";
        }
        return avengerTime + " Giờ trước";
    }
    return `ngày ${y.getDate()} Tháng ${
        y.getMonth() + 1
    } Năm ${y.getFullYear()}`;
}
function ReplyItem(props) {
    const { idComment, id, isRenderComments } = props;
    const user = useSelector((state) => state.user);
    const dispatch = useDispatch();
    const [content, setcontent] = useState("");
    function onSendComment() {
        if (!user) {
            console.log(123);
            toast.error("Vui lòng đăng nhập để bình luận");
            return;
        }

        axiosClient({
            url: `/api/detailproducts/${id}/reply/${idComment}`,
            method: "post",
            data: {
                content: content,
            },
        }).then((data) => {
            setcontent("");
            isRenderComments();
        });
    }
    function onChangeContent(e) {
        setcontent(e.target.value);
    }
    function onClickCancel() {
        dispatch(action.getKey(null));
    }
    return (
        <div>
            <textarea
                name="content"
                rows="5"
                class="comment-input-box"
                value={content}
                onChange={onChangeContent}
            ></textarea>
            <div class="comment-input-nav">
                <div class="comment-input-nav-right">
                    <div class="comment-input__text temp">
                        Quy định đăng bình luận
                    </div>
                </div>
                <div style={{ display: "flex" }}>
                    <div class="comment-input-nav-left" onClick={onClickCancel}>
                        Hủy
                    </div>
                    <div class="comment-input-nav-left" onClick={onSendComment}>
                        Gửi
                    </div>
                </div>
            </div>
        </div>
    );
}
function Reply(props) {
    const { reply } = props;
    return (
        <div className="comment-reply">
            <div className="comment-reply-heading">
                <img
                    src={reply.user.image}
                    alt=""
                    className="commment-avatar"
                />
                <div className="comment-reply-name">{reply.user.username}</div>
            </div>
            <div className="css-826">{reply.content}</div>
            <div className="css-830" style={{ color: "#999" }}>
                {transferDate(reply.createdBy)}
            </div>
        </div>
    );
}
function CommentItem(props) {
    const { comment, isRenderComments, datakey, id } = props;
    const dispatch = useDispatch();
    const keyReply = useSelector((state) => state.reply);
    function onClickReply() {
        dispatch(action.getKey(datakey));
    }
    function renderReply(reply) {
        var temp = [];
        if (reply && reply.data != "") {
            temp = reply.map((item, stt) => <Reply reply={item} key={stt} />);
        }
        return temp;
    }
    return (
        comment && (
            <div className="review">
                <div className="css-820 ">
                    <div className="css-821">
                        <img
                            src={comment.user.image}
                            alt=""
                            className="commment-avatar"
                        />
                        <div className="css-822">{comment.user.username}</div>
                        <div className="css-823">
                            <i className="far fa-check-circle"></i>
                            Đã mua tại G7
                        </div>
                    </div>
                    <div className="css-824">
                        <div className="css-826">{comment.content}</div>
                        <div className="css-825">
                            {generateStar(comment.start)}
                        </div>
                    </div>
                    <div className="css-827">
                        <div className="css-850 m-6" onClick={onClickReply}>
                            Trả lời
                        </div>
                        <div className="sque m-6"></div>
                        <div className="css-830 m-6">
                            {transferDate(comment.createdBy)}
                        </div>
                    </div>
                    {comment.reply && (
                        <div className="List-comment-reply">
                            {renderReply(comment.reply)}
                        </div>
                    )}
                </div>
                {keyReply === datakey && (
                    <ReplyItem
                        id={id}
                        isRenderComments={isRenderComments}
                        idComment={comment.id}
                    />
                )}
            </div>
        )
    );
}

function useQuery() {
    return new URLSearchParams(useLocation().search);
}

function Comment(props) {
    const user = useSelector((state) => state.user);
    const [listComment, setlistComment] = useState(null);
    const [isrenderComment, setisrenderComment] = useState(false);
    const { id } = props;
    const query = useQuery();
    const page = query.get("page") || 1;
    const [comment, setcomment] = useState({
        VoteStart: 5,
        content: "",
    });
    function isRenderComments() {
        setisrenderComment(!isrenderComment);
    }
    const [searchCmt, setsearchCmt] = useState("");
    function onChangeSearchCmt(e) {
        setsearchCmt(e.target.value);
    }
    useEffect(() => {
        axiosClient({
            url: `/api/detailproducts/${id}/comment?page=${page}`,
            method: "get",
        }).then((data) => {
            setlistComment(data);
        });
    }, [isrenderComment, page]);
    function onVoteStar(e) {
        setcomment({
            ...comment,
            VoteStart: parseInt(e.target.getAttribute("data-value")),
        });
    }
    function onChangeContent(e) {
        setcomment({
            ...comment,
            [e.target.name]: e.target.value,
        });
    }
    function onSendComment() {
        if (!user) {
            toast.error("Vui lòng đăng nhập để gửi đánh giá");
        } else {
            axiosClient({
                url: `/api/detailproducts/${id}/comment`,
                method: "post",
                data: {
                    start: comment.VoteStart,
                    content: comment.content,
                },
            }).then((data) => {
                setcomment({
                    VoteStart: 1,
                    content: "",
                });
                setisrenderComment(!isrenderComment);
            });
        }
    }
    function renderComment(comment) {
        var temp = [];
        if (comment && comment.data != "") {
            temp = comment.listComment.map((item, stt) => (
                <CommentItem
                    key={stt}
                    datakey={stt}
                    isRenderComments={isRenderComments}
                    id={id}
                    comment={item}
                />
            ));
        }
        return temp;
    }
    function countAverageStar(listComment) {
        var count = 0;
        if (listComment && listComment.data != "") {
            listComment.forEach((ele) => {
                count += ele.start;
            });
            if (!listComment.length === 0) {
                count = count / listComment.length;
            }
        }
        return count.toFixed(1);
    }
    function countFeedback(star, listComment) {
        var count = 0;
        if (listComment && listComment.data != "") {
            listComment.forEach((ele) => {
                if (ele.start === star) {
                    count++;
                }
            });
        }
        return count;
    }
    return (
        listComment && (
            <div>
                <div class="contain-comment">
                    <div class="css-120"></div>
                    <div class="css-125">
                        <div class="css-126">
                            <ul class="css-127">
                                <li class="css-128">
                                    <span class="css-800">
                                        5 <i class="fas fa-star"></i>
                                    </span>
                                    <div class="css-129">
                                        <div
                                            class="css-802"
                                            style={{
                                                width: `${
                                                    (countFeedback(
                                                        5,
                                                        listComment.listComment
                                                    ) *
                                                        100) /
                                                        listComment.listComment
                                                            .length || 0
                                                }%`,
                                            }}
                                        ></div>
                                    </div>
                                    <span class="css-130">
                                        {countFeedback(
                                            5,
                                            listComment.listComment
                                        )}{" "}
                                        đánh giá
                                    </span>
                                </li>
                                <li class="css-128">
                                    <span class="css-800">
                                        4 <i class="fas fa-star"></i>
                                    </span>
                                    <div class="css-129">
                                        <div
                                            class="css-802"
                                            style={{
                                                width: `${
                                                    (countFeedback(
                                                        4,
                                                        listComment.listComment
                                                    ) *
                                                        100) /
                                                        listComment.listComment
                                                            .length || 0
                                                }%`,
                                            }}
                                        ></div>
                                    </div>
                                    <span class="css-130 have-rating">
                                        {countFeedback(
                                            4,
                                            listComment.listComment
                                        )}{" "}
                                        đánh giá
                                    </span>
                                </li>
                                <li class="css-128">
                                    <span class="css-800">
                                        3 <i class="fas fa-star"></i>
                                    </span>
                                    <div class="css-129">
                                        <div
                                            class="css-802"
                                            style={{
                                                width: `${
                                                    (countFeedback(
                                                        3,
                                                        listComment.listComment
                                                    ) *
                                                        100) /
                                                        listComment.listComment
                                                            .length || 0
                                                }%`,
                                            }}
                                        ></div>
                                    </div>
                                    <span class="css-130">
                                        {countFeedback(
                                            3,
                                            listComment.listComment
                                        )}{" "}
                                        đánh giá
                                    </span>
                                </li>
                                <li class="css-128">
                                    <span class="css-800">
                                        2 <i class="fas fa-star"></i>
                                    </span>
                                    <div class="css-129">
                                        <div
                                            class="css-802"
                                            style={{
                                                width: `${
                                                    (countFeedback(
                                                        2,
                                                        listComment.listComment
                                                    ) *
                                                        100) /
                                                        listComment.listComment
                                                            .length || 0
                                                }%`,
                                            }}
                                        ></div>
                                    </div>
                                    <span class="css-130">
                                        {countFeedback(
                                            2,
                                            listComment.listComment
                                        )}{" "}
                                        đánh giá
                                    </span>
                                </li>
                                <li class="css-128">
                                    <span class="css-800">
                                        1 <i class="fas fa-star"></i>
                                    </span>
                                    <div class="css-129">
                                        <div
                                            class="css-802"
                                            style={{
                                                width: `${
                                                    (countFeedback(
                                                        1,
                                                        listComment.listComment
                                                    ) *
                                                        100) /
                                                        listComment.listComment
                                                            .length || 0
                                                }%`,
                                            }}
                                        ></div>
                                    </div>
                                    <span class="css-130 have-rating">
                                        {countFeedback(
                                            1,
                                            listComment.listComment
                                        )}{" "}
                                        đánh giá
                                    </span>
                                </li>
                            </ul>
                        </div>
                        <div className="feedback_statistic">
                            <h2 className="feedback-statistic-title">
                                ĐÁNH GIÁ TRUNG BÌNH
                            </h2>
                            <div className="feedback-statistic-number">
                                <span>
                                    {countAverageStar(
                                        listComment.listComment
                                    ) || 0}
                                </span>
                                /<span>5</span>
                            </div>
                            <div className="feedback-statistic-star">
                                <div class="stars-outer">
                                    <div
                                        class="stars-inner"
                                        style={{
                                            width: `${
                                                (countAverageStar(
                                                    listComment.listComment
                                                ) *
                                                    100) /
                                                5
                                            }%`,
                                        }}
                                    ></div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <div class="comment">
                    <div class="comment-input">
                        <div className="form-group col-12">
                            <div className="star_rating">
                                <i
                                    data-value={1}
                                    onClick={onVoteStar}
                                    style={
                                        comment.VoteStart >= 1
                                            ? { color: "#F6BC3E" }
                                            : {}
                                    }
                                    className="far fa-star"
                                ></i>
                                <i
                                    data-value={2}
                                    onClick={onVoteStar}
                                    style={
                                        comment.VoteStart >= 2
                                            ? { color: "#F6BC3E" }
                                            : {}
                                    }
                                    className="far fa-star"
                                ></i>
                                <i
                                    data-value={3}
                                    onClick={onVoteStar}
                                    style={
                                        comment.VoteStart >= 3
                                            ? { color: "#F6BC3E" }
                                            : {}
                                    }
                                    className="far fa-star"
                                ></i>
                                <i
                                    data-value={4}
                                    onClick={onVoteStar}
                                    style={
                                        comment.VoteStart >= 4
                                            ? { color: "#F6BC3E" }
                                            : {}
                                    }
                                    className="far fa-star"
                                ></i>
                                <i
                                    data-value={5}
                                    onClick={onVoteStar}
                                    style={
                                        comment.VoteStart >= 5
                                            ? { color: "#F6BC3E" }
                                            : {}
                                    }
                                    className="far fa-star"
                                ></i>
                            </div>
                        </div>
                        <textarea
                            name="content"
                            rows="5"
                            class="comment-input-box"
                            value={comment.content}
                            onChange={onChangeContent}
                        ></textarea>
                        <div class="comment-input-nav">
                            <div class="comment-input-nav-right">
                                <div class="comment-input__text temp">
                                    Quy định đăng bình luận
                                </div>
                            </div>
                            <div
                                class="comment-input-nav-left"
                                onClick={onSendComment}
                            >
                                Gửi
                            </div>
                        </div>
                    </div>
                    <div class="comment-question">
                        <div class="comment-question__header">
                            <div class="comment-question__count">
                                {listComment.data === "" ||
                                listComment.listComment.length === 0
                                    ? "Chưa có bình luận nào"
                                    : `Có ${listComment.totalComment} bình luận`}
                            </div>
                            {listComment.data === "" ||
                            listComment.listComment.length === 0 ? (
                                ""
                            ) : (
                                <div class="comment-question__search">
                                    <i class="fas fa-search comment-question__search-icon"></i>
                                    <input
                                        type="text"
                                        placeholder="Tìm theo nội dung, người gửi, ...."
                                        class="comment-question__search-input"
                                        value={searchCmt}
                                        onChange={onChangeSearchCmt}
                                    />
                                </div>
                            )}
                        </div>
                        {renderComment(listComment)}
                    </div>
                    {listComment.data === "" ||
                    listComment.listComment.length === 0 ? (
                        ""
                    ) : (
                        <PaginationComment
                            totalPage={listComment.totalPage}
                            crrpage={parseInt(page)}
                            search={searchCmt}
                            id={id}
                        />
                    )}
                </div>
            </div>
        )
    );
}

export default Comment;
