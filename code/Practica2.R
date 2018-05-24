#
# LOADING DATA
#
# Read csv and store it in a matrix
youtubes_raw <- read.csv("C:/uoc/Semestre 1/Tipologia i cicle de vida de les dades/Practica2/CAvideos.csv");
# subset of selected columns
youtubes <- youtubes_raw[, c(1,2,5,8,9,10,11,13,14,15)];
# check for NA and keep only meaningfull rows
youtubes_clean <- youtubes[is.na(youtubes$video_id) == 0,];
# summarize resulting data
summary(youtubes_clean);

#
# ZERO VALUES on likes, dislikes and comment_count
#
# check wether ratings_disabled == True have 0 likes/dislikes
youtubes_clean_no_likes <- youtubes_clean[youtubes_clean$ratings_disabled == "True",];
summary(youtubes_clean_no_likes)
# get subset with ratings enabled
youtubes_clean_likes <- youtubes_clean[youtubes_clean$ratings_disabled == "False",];
# calc mean
like_mean <- mean(youtubes_clean_likes$likes);
# compare with general mean to check results
mean(youtubes_clean$likes);
# update ratings_disabled values
youtubes_clean <- within(youtubes_clean, likes[ratings_disabled == "True"] <- like_mean);
# repeat for dislikes atribute
dislike_mean = mean(youtubes_clean_likes$dislikes);
youtubes_clean <- within(youtubes_clean, likes[ratings_disabled == "True"] <- dislike_mean);
# repeat for comment_count
youtubes_clean_no_comments <- youtubes_clean[youtubes_clean$comments_disabled == "False",];
comments_mean <- mean(youtubes_clean_no_comments$comment_count);
youtubes_clean <- within(youtubes_clean, comment_count[comments_disabled == "True"] <- comments_mean);
mean (youtubes_clean_no_comments$comment_count);
mean (youtubes_clean$comment_count);
# check wether we still have zero values
youtubes_still_zeros <- youtubes_clean[youtubes_clean$likes == 0 | youtubes_clean$dislikes == 0 | youtubes_clean$comment_count == 0,];
summary(youtubes_still_zeros);

#
# INCOMPLETE DATA: remove video_error_or_removed = True
#
youtubes_clean_no_error <- youtubes_clean[youtubes_clean$video_error_or_removed == "False",];
summary(youtubes_clean_no_error);
# remove meaningless column video_error_or_removed
youtubes_clean_no_error <- youtubes_clean_no_error[, c(1,2,3,4,5,6,7,8,9)];

#
# OUTLINERS
#
# graphic representation for each attribute
hist(youtubes_clean_no_error$views, main="Views");
hist(youtubes_clean_no_error$likes, main="Likes");
hist(youtubes_clean_no_error$dislikes, main="Dislikes");
hist(youtubes_clean_no_error$comment_count, main="Comments");

# set a short alias for our dataset
yf <- youtubes_clean_no_error;

#
# ADD DISCRETE COLUMNS
#
yf$d_views="Mitja";
# lower quad
yf <- within(yf, d_views[yf$views <= 137248 ] <- "Baix");
# upper quad
yf <- within(yf, d_views[yf$views >= 937059 ] <- "Alt");
yf$d_likes="Mitja";
# lower quad
yf <- within(yf, d_likes[yf$likes <= 2059 ] <- "Baix");
# upper quad
yf <- within(yf, d_likes[yf$likes >= 28189 ] <- "Alt");
yf$d_dislikes="Mitja";
# lower quad
yf <- within(yf, d_dislikes[yf$dislikes <= 92 ] <- "Baix");
# upper quad
yf <- within(yf, d_dislikes[yf$dislikes >= 933 ] <- "Alt");
yf$d_comment_count="Mitja";
# lower quad
yf <- within(yf, d_comment_count[yf$comment_count <= 426 ] <- "Baix");
# upper quad
yf <- within(yf, d_comment_count[yf$comment_count >= 3889 ] <- "Alt");

#
# Check normality
#
# views
qqnorm(yf[,4]);
qqline(yf[,4],col="red");
shapiro.test(yf[sample(nrow(yf), 5000), "views"]);
# likes
qqnorm(yf[,5]);
qqline(yf[,5],col="red");
shapiro.test(yf[sample(nrow(yf), 5000), "likes"]);
# dislikes
qqnorm(yf[,6]);
qqline(yf[,6],col="red");
shapiro.test(yf[sample(nrow(yf), 5000), "dislikes"]);
# comment count
qqnorm(yf[,7]);
qqline(yf[,7],col="red");
shapiro.test(yf[sample(nrow(yf), 5000), "comment_count"]);

#
# Create month column
#
yf$month=paste(substr(yf[,"trending_date"],1,2),substr(yf[,"trending_date"],7,9),sep="");

#
# Check correlation
#
# views vs likes
cor.test(yf$views,yf$likes,method="pearson");
# views vs dislikes
cor.test(yf$views,yf$dislikes,method="pearson");
# views vs comment_count
cor.test(yf$views,yf$comment_count,method="pearson");

#
# Linear Regresion to check dependency between atributes
#
# init seed
set.seed(1);
# calc sample sizes
yf_random <- yf[sample( nrow( yf ) ),];
# first 80%
train <- yf_random[1:26470,];
# last 20%
test <- yf_random[26471:33088,];

# check regression models
# views vs category_id
model<-lm(formula = views ~ category_id, data=train);
summary(model);
prob<-predict(model, test, type="response");
check<-data.frame(
  real=test$views,
  predicted= prob,
  dif=ifelse(test$views>prob, -prob*100/test$views,prob*100/test$views)
  );
colnames(check)<-c("Real","Prediction","Dif%");
summary(check);

# create numeric date column
train$n_date = as.numeric(substr(train[,"trending_date"],1,2))*10000+
	as.numeric(substr(train[,"trending_date"],7,9))*100+
	as.numeric(substr(train[,"trending_date"],4,5));
test$n_date = as.numeric(substr(test[,"trending_date"],1,2))*10000+
	as.numeric(substr(test[,"trending_date"],7,9))*100+
	as.numeric(substr(test[,"trending_date"],4,5));

# views vs numeric trending_date
model<-lm(formula = views ~ n_date, data=train);
summary(model);
prob<-predict(model, test, type="response");
check<-data.frame(
  real=test$views,
  predicted= prob,
  dif=ifelse(test$views>prob, -prob*100/test$views,prob*100/test$views)
  );
colnames(check)<-c("Real","Prediction","Dif%");
summary(check);

# views vs category_id + numeric trending_date
model<-lm(formula = views ~ category_id + n_date, data=train);
summary(model);
prob<-predict(model, test, type="response");
check<-data.frame(
  real=test$views,
  predicted= prob,
  dif=ifelse(test$views>prob, -prob*100/test$views,prob*100/test$views)
  );
colnames(check)<-c("Real","Prediction","Dif%");
summary(check);


#
# Grouped Bar Plot
#
# by category_id
counts <- table(yf$d_views, yf$category_id);
barplot(counts, main="Trending Topic Videos by Category", xlab="Number of Views", col=c("green","red", "blue"), legend = rownames(counts), beside=TRUE);
# by month
counts <- table(yf$d_views, yf$month);
barplot(counts, main="Trending Topic Videos by Month", xlab="Number of Views", col=c("green","red", "blue"), legend = rownames(counts), beside=TRUE);

