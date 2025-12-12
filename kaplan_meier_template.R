#===========================================библиотеки====================================================
library(dplyr)
library(readxl)
library(ggsurvfit)
library(survival)
library(scales)

#=============================веротяность развития хрРТПХ после ТКМ========================================

df <- read_excel('*****') %>%
    mutate(`Дата ТКМ` = as.Date(`Дата ТКМ`),
        `Дата хрРТПХ` = as.Date(`Дата хрРТПХ`),
        `Дата последнего контакта` = as.Date(`Дата последнего контакта`)
        )

#====================================проверка данных========================================================

names(df) 
    # -> возвращает список всех названий колонок в датафрейме или векторе
print(head(df))
    # -> проверка первых 5 строк

#======================================подготовка===========================================================
#данных кавычки `` (столбцы с пробелами ) и ''/"" (текст) абсолютно разные вещи

data_gvhd <- df%>%
    mutate(
        #шаг 1: выбираем дату окончания
        end_date = if_else(`ХрРТПХ` == 1, `Дата хрРТПХ`,`Дата последнего контакта`),
        #шаг 2: разница двух дат    
        time_gvhd = as.numeric(difftime(end_date, `Дата ТКМ`, units = "days")),
        event_gvhd = ifelse(`ХрРТПХ` == 1 & !is.na(`Дата хрРТПХ`), 1, 0),
        IST_group = factor(`Тимоглобулин`, levels = c(0, 1),
        labels = c("кАТГ-", "кАТГ+"))) %>%
                filter(time_gvhd >= 0, !is.na(IST_group)) #два фильтра -> время от ТКМ до хрРТПХ не отрицательное
                                                           #-> больной точно отнесен к одной их двух групп

print(paste("Пациентов после фильтра:", nrow(data_gvhd)))
print(table(data_gvhd$IST_Group), table(data_gvhd$event_gvhd))


#=======================================строим график=====================================================

fit_gvhd <- survfit2(Surv(time_gvhd, event_gvhd) ~ IST_group, data = data_gvhd) %>%
        # survfit2(Surv(OS_in_days,Death_event) ~ IST_Group, data = cy_group_years) %>% 
            #создаем Камплан-Мейер: время (time_gvhd), событие (event_gvhd) по группам (IST_group)
    ggsurvfit(linetype_aes = TRUE, type = "risk", theme = theme_ggsurvfit_KMunicate()) +
            #ggsurvfit(linetype_aes = TRUE) 
                #преобразуем в ggplot с разными линиями для групп
    scale_color_manual(values = c("кАТГ-" = "#1f77b4", 
                                "кАТГ+" = "#ff7f0e")) +
                #меняем цвета линий в ручную
    scale_fill_manual(values = c("кАТГ-" = "#1f77b4", "кАТГ+" = "#ff7f0e")) +
                #меняем цвета заливки в ручную
    add_confidence_interval() +
        #add_confidence_interval() +
                #добавляет 95% доверительный интервал
    add_censor_mark() +
            #add_censor_mark() +
                #добавляет"+" для цензурирования
    #add_risktable(risktable_stats = c("n.risk", "cum.event", "cum.censor")) +
            #add_risktable(risktable_stats = c("n.risk", "cum.event", "cum.censor")) +
                #таблица риска снизу: число в риске, события, цензура по времени
    add_risktable_strata_symbol(symbol ="\U25CF", size = 10) +
            #add_risktable_strata_symbol(symbol = "\U25CF", size = 10)+
                #точки в таблице для групп (визуальное разделение)
    add_quantile(y_value = 0.5, linetype = "dotted") +
            #add_quantile(y_value = 0.5, linetype = "dotted") +
                #медиана выживаемости (50% линия по середине)
    add_pvalue("annotation", size = 5) +
            #add_pvalue("annotation", size = 5) +
                #p-value log-rank
    labs(x ="время после алло-ТГСК, дни", y = "вероятность развития хрРТПХ") +  #title = "хрРТПХ: тимоглобулин vs контроль") +
            #labs(x = "Дни после алло-ТГСК", y = "Вероятность",title = "2024 - по н.в")+
                #подписи осей и заголовок
#   theme_ggsurvfit_KMunicate() +
            #theme_ggsurvfit_KMunicate() +
                #кастомная тема
     scale_y_continuous(limits = c(0, 100)) +
            #scale_y_continuous(limits = c(0, 100)) + 
                # ось Y от 0 до 100
    scale_x_continuous(expand = c(0.02, 0)) +
            #scale_x_continuous(expand = c(0.02, 0)) +
                #убирает лишние отступы по Х
    scale_y_continuous(labels = scales::label_percent()) +
            #scale_y_continuous(labels = scales::label_percent())+ 
                #формат Y в % а не десятичные дроби
    scale_ggsurvfit() +
            #scale_ggsurvfit()+
                #ljg yfcnhjqrb lkz ыгкмаше
    theme(legend.position = "bottom",
            legend.box = "horizontal",
            #plot.title = element_text(hjust = 0.5, size = 25, face = "bold"), # -> высота, шрифт
            axis.title = element_text(size = 20), # подписи осей
            axis.text = element_text(size = 18), # деления осей
            legend.title = element_text(size = 20), #заголовок легенды
            legend.text = element_text(size = 18))
            #theme(legend.position.inside = c(0.85, 0.85))
                #легенда внутри графика


print(fit_gvhd)
ggsave("*******", width = 10, height = 8, dpi = 300)