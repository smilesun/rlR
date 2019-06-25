makeCompactableNetTF = function(state_dim, act_cnt) {
 hun = 10L
 requireNamespace(tensorflow)
 input =  tf$placeholder(tf$float32, shape(NULL, state_dim))
 W = tf$Variable(tf$zeros(shape(state_dim, hun)))
 b = tf$Variable(tf$zeros(shape(hun)))
 hidden = tf$nn$relu(tf$matmul(input, W) + b)
 w_critic = tf$Variable(tf$zeros(shape(hun, 1L)))
 b_critic = tf$Variable(tf$zeros(shape(1L)))
 w_actor = tf$Variable(tf$zeros(shape(hun, act_cnt)))
 b_actor = tf$Variable(tf$zeros(shape(act_cnt)))
 critic = tf$matmul(hidden, w_critic) + b_critic
 actor = tf$matmul(hidden, w_actor) + b_actor
 w_critic = tf$Variable(tf$zeros(shape(hun, 1L)))
 b_critic = tf$Variable(tf$zeros(shape(1L)))
 #loss_critic <- tf$reduce_mean(0.5 * (critic - critic_target) ^ 2)
}


