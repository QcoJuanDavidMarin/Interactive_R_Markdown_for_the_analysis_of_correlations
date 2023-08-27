import streamlit as st
import streamlit.components.v1 as components
import pandas as pd
import plotly.express as px
import plotly.graph_objs as go
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from factor_analyzer.factor_analyzer import calculate_kmo, calculate_bartlett_sphericity
from sklearn.decomposition import PCA
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from bioinfokit.visuz import cluster
from sklearn.cluster import KMeans
from scipy import stats
from scikit_posthocs import posthoc_tukey
from statannotations.Annotator import Annotator


st.set_page_config(page_title='Correlation Analysis in Physico-Chemical Experiments_JDM',
                    page_icon='👁',layout="wide")

def main():
	tab0, tab1, tab2, tab3, tab4 = st.tabs(['Loaded data','basic statistics', 'ACP', 'Anova post hoc tests', 'Lineal Models'])
	tab0.title("Correlation Analysis in Physico-Chemical Experiments")

	tab0.subheader(':blue[Streamlit app Developed by:] **Juan David Marín**')
	tab0.markdown("LinkInd: [link](https://www.linkedin.com/in/qco-juan-david-marin/)")
	tab0.markdown("Blog Medium: [link](https://medium.com/@qcojuandavidmarin)")
## TAB0
## Create upload data side
	uploadData = st.sidebar.file_uploader(label='Upload Data', type = ['csv'])
	if uploadData is not None:
		try:
			file = pd.read_csv(uploadData, sep=';')
			tab0.dataframe(file.style.highlight_max(axis=0))
		except Exception as e:
			st.error("Error al cargar el archivo CSV: {}".format(e))

## TAB 1 

	tab1.subheader('Basics statistics')
	tab1.dataframe(file.describe().style.highlight_max(axis=0))

## Boxplot numerical variables
		## Numerical vars
	lista_num = list(file.select_dtypes(include=['float64', 'int']).columns)
		## strings var 
	lista_str = list(file.select_dtypes(include=['object']).columns)
	col1, col2 = tab1.columns(2, gap = "large")
	with col1:
		tab1.select_numvars = st.multiselect(label ='Select Num. Variables', 
			      options=lista_num, default= lista_num)
	with col2:
		tab1.select_strvars = st.selectbox(label = 'Select Cat. Variables',
				     options = lista_str)

	file_melt = pd.melt(frame=file, id_vars=lista_str, value_vars=lista_num)
	file_melt2 = file_melt[file_melt['variable'].isin(tab1.select_numvars)]
	## BoxPlot categorical variables
	boxplot = px.box(file_melt2, x = 'variable', y = 'value', color = tab1.select_strvars,
		points="all",template = "simple_white")
	tab1.plotly_chart(boxplot, use_container_width=True)
	## Plot categorical variable
	expander = tab1.expander('**Show Cat. Var. Distri.**', expanded=False)
	fig, ax = plt.subplots()
	ax = file[[tab1.select_strvars]].value_counts().plot(kind='bar', color=['red','green'],title='Markets', figsize=(5,2))
	expander.pyplot(fig, use_container_width=False)
	## Plot correlation
	expandercor = tab1.expander('**Show Correlation amog vars**')
	figcor = plt.figure(figsize=(10, 4))
	corr = file[lista_num].corr(method = 'pearson')
	mask= np.zeros_like(corr)
	mask[np.triu_indices_from(mask)] = True
	sns.heatmap(corr, annot = True,cmap=sns.diverging_palette(20,220,as_cmap=True),mask=mask)
	expandercor.pyplot(figcor)


	#### PAG PCA TAB2 #####
	tab2.warning('Principal component Analisys')
	col_bt, col_kmo = tab2.columns([1,3.5], gap='large')
	with col_bt:	
		bt = calculate_bartlett_sphericity(file[lista_num])
		col_bt.markdown('Bartlett’s Test')
		col_bt.metric(label='Bartlett’s Test', value=round(bt[0],2), delta=bt[1])
	with col_kmo:
		col_kmo.markdown('Kaiser-Meyer-Olkin (KMO)')
		kmo = calculate_kmo(file[lista_num])
		kmo_pd = pd.DataFrame([kmo[0]], columns=file[lista_num].columns)
		kmo_pd = pd.concat([kmo_pd, pd.DataFrame([kmo[1]])], axis=1).rename(columns= {0:'Overal'})
		col_kmo.dataframe(kmo_pd.style.highlight_max(axis=1), hide_index=True, width=1400)

	## Principal component analisys SLIDER
	npca = st.sidebar.slider('Select PCA', 1,10, value = 3)
	## kMEANS SLIDER
	kmeanslider = st.sidebar.slider('Select n° K', 1,10, value=3)
	## Sacale data
	sc = StandardScaler()
	datos_numeric_sc = sc.fit_transform(file[lista_num])
	## Apply pca
	pca = PCA(n_components=npca, random_state=123)
	modelo_pca = pca.fit_transform(datos_numeric_sc)
	## Apply Kmeans
	kmeans = KMeans(n_clusters=kmeanslider, random_state=123, n_init=10)
	km = kmeans.fit(datos_numeric_sc)
	

	pcdf1 = pd.DataFrame(modelo_pca)
	pcanames = ['pc' + str(i) for i in np.arange(1, pcdf1.shape[1]+1)]
	pcanames = dict(zip(pcdf1.columns, pcanames))
	pcdf1 = pcdf1.rename(columns=pcanames)
	pckmeandf = pd.concat([file, pcdf1], axis=1)
	pckmeandf['kmean'] = pd.DataFrame(km.labels_).astype(str)


	pcaDF = pd.DataFrame(
    data    = pca.components_,
    columns = lista_num,
    index = ['PC' + str(i) for i in  np.arange(1,len(pca.explained_variance_ratio_)+1)]
	)

	expander_dfpca = tab2.expander('Show PCA DF')
	expander_dfpca.dataframe(pcaDF.style.highlight_max(axis=1), width=1000)

	tab2.metric('Explained_Variance_Ratio', value=round(sum(pca.explained_variance_ratio_),4))
	
	### Graph explained_variance_ratio_
	df_varianza = pd.concat([
    pd.DataFrame(np.cumsum(np.round(pca.explained_variance_ratio_, decimals=4))).rename(columns={0:'Cumulative_Variance'}),
    pd.DataFrame(pca.explained_variance_ratio_).rename(columns={0:'Explained_Variance'}),
	], axis=1)
	df_varianza['PCA'] = ['PC' + str(i) for i in  np.arange(1,len(pca.explained_variance_ratio_)+1)]

	col_pca_var1, col_pca_grap = tab2.columns([1,2], gap = "large")
	with col_pca_var1:
		col_pca_var1.dataframe(df_varianza)

	with col_pca_grap:
		fig2 = go.Figure()
		fig2.add_trace(
			go.Bar(
				x=df_varianza['PCA'],
				y=df_varianza['Explained_Variance'],
				name='Explained',
				marker=dict( color = 'LightSeaGreen')
			)
		)
		fig2.add_trace(
			go.Scatter(
				x=df_varianza['PCA'],
				y=df_varianza['Cumulative_Variance'],
				name='Explained',
				marker=dict( color = 'RoyalBlue')
			)			
		)
		fig2.update_yaxes(title_text='Value (%)')
		col_pca_grap.plotly_chart(fig2)

	# PCA biplot 
	lista_str2 = list(pckmeandf.select_dtypes(include=['object']).columns)

	expander_pca = tab2.expander('Show Varibles correlation')
	colpca_1, colpca_2 = expander_pca.columns(2, gap = "large")
	with colpca_1:
		expander_pca.select_numvars = st.multiselect(label ='Select Cat. Variables Biplot', 
			      options=lista_num, default= lista_num[5])
	with colpca_2:
		expander_pca.select_strvars = st.selectbox(label = 'Select Cat. Variables Biplot',
				     options = lista_str2)


	features = pckmeandf[expander_pca.select_numvars].columns.values
	fig_num = px.scatter_matrix(
		pckmeandf,
		hover_data= pckmeandf.iloc[:,[0]],
		dimensions=features,
		color=expander_pca.select_strvars)
	fig_num.update_traces(diagonal_visible=False)
	expander_pca.plotly_chart(fig_num, use_container_width=True)

	## PCAKMEAN biplot
	charges = expander_pca.checkbox('Show Loadings')

	if charges:
		features1 = file[lista_num].columns
		loadings = pca.components_.T *  np.sqrt(pca.explained_variance_)

		figbi = px.scatter(pckmeandf, x = 'pc1', y = 'pc2',   color=expander_pca.select_strvars,
						hover_name='sample', template = "simple_white")

		for i, feature in enumerate(features1):
			figbi.add_shape(
				type='line',
				x0=0, y0=0,
				x1=loadings[i, 0]*4.5,
				y1=loadings[i, 1]*4.5,
				opacity=0.5,
				line=dict(color="brown",width=1)
			)

			figbi.add_annotation(
				x=loadings[i, 0]*4.5,
				y=loadings[i, 1]*4.5,
				ax=0, ay=0,
				xanchor="center",
				yanchor="bottom",
				text=feature,
				name = 'JDM',
				textangle=-30,
				opacity=0.7,
				font=dict(color="black", size=12)
			)
	else:
		figbi = px.scatter(pckmeandf, x = 'pc1', y = 'pc2',   color=expander_pca.select_strvars,
						hover_name='sample', template = "simple_white", text='sample')
	expander_pca.plotly_chart(figbi,use_container_width=True)

	#### PAG PCA TAB3 #####
	
	tab3.warning('Plotting post hoc tests')
	tab3.select_numvars_posthoc = tab3.selectbox(label ='Select Cat. post hoc tests', 
			      options=lista_num)

	tukey_df = posthoc_tukey(pckmeandf, val_col=tab3.select_numvars_posthoc, group_col='kmean')
	remove = np.tril(np.ones(tukey_df.shape), k=0).astype("bool")
	tukey_df[remove] = np.nan
	molten_df = tukey_df.melt(ignore_index=False).reset_index().dropna()
	
	## Box plot post hoc 
	fig2, ax2 = plt.subplots()
	fig2 = plt.figure(figsize=(10, 4))
	ax2 = sns.boxplot(data=pckmeandf, x="kmean", y=tab3.select_numvars_posthoc, order = pckmeandf['kmean'].unique(), showfliers=True)
	pairs = [(i[1]["index"], i[1]["variable"]) for i in molten_df.iterrows()]
	p_values = [i[1]["value"] for i in molten_df.iterrows()]
	annotator = Annotator(
		ax2, pairs, data=pckmeandf, x="kmean", y=tab3.select_numvars_posthoc, order=pckmeandf['kmean'].unique()
	)
	sns.stripplot(data=pckmeandf, x="kmean", y=tab3.select_numvars_posthoc, order = pckmeandf['kmean'].unique(), color = 'black',alpha = 0.5)
	sns.pointplot(data=pckmeandf, x="kmean", y=tab3.select_numvars_posthoc, order = pckmeandf['kmean'].unique(), linestyles='--', scale=0.4, 
				color='k', errwidth=0, capsize=0, ax=ax2)

	annotator.configure(text_format='star', verbose=1, loc="inside",line_height=0.01, text_offset=1)
	annotator.set_pvalues_and_annotate(p_values)
	fig2.suptitle('post hoc tests Tukey’s Honest')
	tab3.pyplot(fig2)

	expander_pca = tab3.expander('Tukey’s Honest Significant Differences')
	expander_pca.dataframe(molten_df, hide_index=True)


	# tab4 lINEAL MODELS
	collm1, collm2, colpred = tab4.columns(3, gap='large')
	with collm1:
		x_var_lm = collm1.selectbox(label ='Select var X', 
			      	options=lista_num)
	with collm2:
		y_var_lm = collm2.selectbox(label ='Select var Y', 
		 			options=lista_num)
	with colpred:
		predict_value = colpred.number_input('Insert value to predict')
		
	X = pckmeandf[x_var_lm].values.reshape(-1,1) 
	y = pckmeandf[y_var_lm].values  

	model = LinearRegression()
	model.fit(X, y)

	new_measure = np.array([predict_value]).reshape(-1, 1)
	predicted_new_measure = model.predict(new_measure)
	slope = model.coef_[0]
	intercept = model.intercept_
	equation = f'Ecuation: y = {slope:.2f}x + {intercept:.2f}'
	fig3,ax3 = plt.subplots()
	fig3 = plt.figure(figsize=(10, 4))
	ax3 = plt.scatter(X, y, color='blue', label='Real Data')
	ax3 = plt.plot(X, model.predict(X), color='red', linewidth=1, linestyle = '-.',label='Lineal regression')
	ax3 = plt.scatter(new_measure, predicted_new_measure, color='green', marker='X', s=100, label='Prediction')
	ax3 = plt.title(f'linear regression {x_var_lm} Vs {y_var_lm}')
	ax3 = plt.xlabel(f'{x_var_lm}')
	ax3 = plt.ylabel(f'{y_var_lm}')
	# Add the equation of the line to the legend
	ax3 =legend_label = f'{equation}'
	ax3 = plt.legend([legend_label, 'Real Data', 'Prediction'])
	tab4.metric(label = 'Predicted value', value = np.round(predicted_new_measure,2))
	tab4.pyplot(fig3)
	
if __name__ == '__main__':
	main()
