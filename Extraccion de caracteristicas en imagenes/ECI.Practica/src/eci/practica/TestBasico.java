package eci.practica;

import java.io.File;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.CvType;
import org.opencv.core.MatOfFloat;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.Size;
import org.opencv.features2d.FastFeatureDetector;
import org.opencv.features2d.Features2d;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.ml.Ml;
import org.opencv.ml.SVM;
import org.opencv.objdetect.HOGDescriptor;


public class TestBasico {

    // Necesario para cargar la biblioteca nativa de OpenCV
    static{ System.loadLibrary(Core.NATIVE_LIBRARY_NAME); }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        System.out.println("Versión OpenCV " + Core.VERSION);
        
        //Leemos imagen
        String strImgPrueba = "Fry.jpg";                    
        Mat image = Imgcodecs.imread(strImgPrueba, Imgcodecs.CV_LOAD_IMAGE_COLOR);
        System.out.println("Tamaño imagen "+image.size());      
        
        //Obtenemos una versión en nivel de gris y otra suavizada
        Mat gris = new Mat(image.width(), image.height(), image.type());
        Mat blur = new Mat(image.width(), image.height(), image.type());
        Imgproc.cvtColor(image, gris, Imgproc.COLOR_RGB2GRAY);
        Imgproc.blur(gris, blur, new Size(3, 3));

        // 1. ------ Canny  
        // 
        Mat canny = new Mat(image.width(), image.height(), image.type());        
        Imgproc.Canny(blur, canny, 10, 100);
        
        // 2. ------ Harris
        //
        Mat harris = new Mat( image.width(), image.height(),image.type());
        // La API dice que src tiene que ser flotante, pero no da excepción
        // blur.convertTo(gris, CvType.CV_32FC1); 
        Imgproc.cornerHarris(gris, harris, 2, 3, 0.04);
        //Imgproc.dilate(harris, harris, new Mat()); // Resalta (no necesario)
        // Para visualizar, normalizo entre 0 y 255
        Core.normalize(harris, harris, 0, 255, Core.NORM_MINMAX);               
        Core.convertScaleAbs(harris, harris);
        // Umbralizo (no necesario, solo para ver los valores mayores)
        //Mat harris_bin = new Mat( image.width(), image.height(),image.type());
        //Imgproc.threshold(harris, harris_bin, 128, 255, Imgproc.THRESH_BINARY);
        //HighGui.namedWindow("Harris Binary", HighGui.WINDOW_AUTOSIZE);
        //HighGui.imshow("Harris Binary", harris_bin);
        
        // 3. ------ HOG
        //
        // Constructor por defecto. 
        // block size: 16x16, window size: 64x128, stride size: 8x8, 
        // cell size: 8x8, number of bins: 9, descriptor size: 3780
        HOGDescriptor hog = new HOGDescriptor(); 
        
        //Otra opcion sería indicar los tamaños de celda, bloque, etc.
        //Size winSize = new Size(32,128);
        //Size cellSize = new Size(8,8);                    
        //Size blockSize = new Size(16,16);    
        //Size blockStride = new Size(8,8);    
        //int nBinsOrientacion = 9;        
        //hog = new HOGDescriptor(winSize,blockSize,blockStride,cellSize,nBinsOrientacion);         
        
        MatOfFloat descriptors = new MatOfFloat();
        hog.compute(image, descriptors);
        System.out.println("HOG ("+descriptors.size()+"): "+
                "block size: "+hog.get_blockSize()+
                ", window size: "+hog.get_winSize()+
                ", stride size: "+hog.get_blockStride()+
                ", cell size: "+hog.get_cellSize()+
                ", number of bins: "+hog.get_nbins()+
                ", descriptor size: "+hog.getDescriptorSize());
                
        
        // 4. ------ SIFT
        //
        // La forma de crear los puntos SIFT sería la que se muestra a 
        // continuación, pero desde la versión 3.x no está en la distribución 
        // básica. Los descriptores SIFT, así como otros que están protegidos
        // por derechos de autor, se encuentran en la extensión 'opencv_contrib'
        // (es necesario compilar a partir de las fuentes, no dispotible el.jar)
        // 
        // SIFT fd = SIFT .create();

        // A modo de ejemplo, se calculan otros puntos sí disponibles en la 
        // distribución básica
        FastFeatureDetector fd = FastFeatureDetector.create();
        MatOfKeyPoint sift = new MatOfKeyPoint();
        fd.detect(gris, sift);
        Mat siftImage=new Mat();        
        Features2d.drawKeypoints(gris, sift, siftImage );
                
        //Mostramos resultados
        HighGui.namedWindow(strImgPrueba, HighGui.WINDOW_AUTOSIZE);
        HighGui.imshow(strImgPrueba, image);
        HighGui.namedWindow("Canny", HighGui.WINDOW_AUTOSIZE);
        HighGui.imshow("Canny", canny);
        HighGui.namedWindow("Harris", HighGui.WINDOW_AUTOSIZE);
        HighGui.imshow("Harris", harris);        
        HighGui.namedWindow("SIFT", HighGui.WINDOW_AUTOSIZE);
        HighGui.imshow("SIFT", siftImage);
        HighGui.waitKey(0);

        // 5. ------ Clasificación
        //
        ejemploClasificadorImagenes();
    }
    
    private static final String PATH_POSITIVE_TRAIN = "data/train/pedestrians/"; 
    private static final String PATH_NEGATIVE_TRAIN = "data/train/background/";
    private static final String PATH_POSITIVE_TEST = "data/test/pedestrians/"; 
    private static final String PATH_NEGATIVE_TEST = "data/test/background/";
    private static final String EXAMPLE_POSITIVE = PATH_POSITIVE_TEST + "AnnotationsPos_0.000000_crop_000011b_0.png"; 
    private static final String EXAMPLE_NEGATIVE = PATH_NEGATIVE_TEST+"AnnotationsNeg_0.000000_00000002a_0.png";

    /**
     * Prueba de entrenamiento de un clasificador.
     */
    static public void ejemploClasificadorImagenes(){  
        //Obtenemos los datos para el entrenamiento del clasificador
        Mat trainingData = new Mat();
        Mat classes = new Mat();
        loadTrainingData(trainingData, classes);       
        //Entrenamos el clasificador
        SVM clasificador = train(trainingData, classes);
        System.out.println("Clasificador entrenado");             
        //Leemos imagen a clasificar
        Mat image = Imgcodecs.imread(EXAMPLE_POSITIVE, Imgcodecs.CV_LOAD_IMAGE_COLOR);               
        if (image.empty()) {
            System.err.println("Cannot load image ");
        }        
        //Clasificamos
        float prediccion = test(image, clasificador);
        System.out.println("Predicción: " + prediccion);
    }
    
    /**
     * Clasifica la imagen pasada por parámetro
     * @param image imagen a clasificar
     * @param clasificador clasificador
     * @return clase a la que pertenece la imagen (1|0) 
     */
    protected static float test(Mat image, SVM clasificador) {
        //HOG de la imagen a testear
        HOGDescriptor hog = new HOGDescriptor();
        MatOfFloat descriptor = new MatOfFloat();
        hog.compute(image, descriptor);
        //Clasificación
        return clasificador.predict(descriptor.reshape(1, 1));       
    }
    
    /**
     * Entrena el clasificador
     * @param trainingData datos de entrenamiento
     * @param classes clases asocidas a los datos de entrenamiento
     * @return un clasificador SVM
     */
    protected static SVM train(Mat trainingData, Mat classes) {
        SVM svm = SVM.create();
        svm.setKernel(SVM.LINEAR);
        svm.setType(SVM.C_SVC);
        svm.train(trainingData, Ml.ROW_SAMPLE, classes);
        return svm;
    }
    
    /**
     * Lee las imágenes de entrenamiento (positivas y negativas) y calcula sus
     * descriptores para el entrenamiento.
     *
     * @param trainingData matriz donde almacenará los datos de entrenamiento
     * @param classes matriz donde almacenará las clases asociaas a los datos de
     * entrenamiento
     */
    protected static void loadTrainingData(Mat trainingData, Mat classes) {
        File listFiles[];
        
        //Casos positivos
        listFiles = new File(PATH_POSITIVE_TRAIN).listFiles();
        for (File file : listFiles) {
            Mat img = Imgcodecs.imread(PATH_POSITIVE_TRAIN + file.getName(), Imgcodecs.CV_LOAD_IMAGE_COLOR);
            //HOG
            HOGDescriptor hog = new HOGDescriptor();
            MatOfFloat descriptor = new MatOfFloat();
            hog.compute(img, descriptor);
            //Incorporamos el descriptor a la matriz de entrenamiento y la 
            //etiqueta '1' a la matriz de etiquetas asociada
            trainingData.push_back(descriptor.reshape(1, 1)); //Vector fila; flotante
            classes.push_back(Mat.ones(new Size(1, 1), CvType.CV_32SC1)); // Vector de un elemento; entero
        }
        System.out.println("Leidas " + listFiles.length + " imágenes de entrenamiento -> positivas");
        
        //Casos negativos
        listFiles = new File(PATH_NEGATIVE_TRAIN).listFiles();
        for (File file : listFiles) {
            Mat img = Imgcodecs.imread(PATH_NEGATIVE_TRAIN + file.getName(), Imgcodecs.CV_LOAD_IMAGE_COLOR);
            //HOG
            HOGDescriptor hog = new HOGDescriptor();
            MatOfFloat descriptor = new MatOfFloat();
            hog.compute(img, descriptor);
            //Incorporamos el descriptor a la matriz de entrenamiento y la 
            //etiqueta '0' a la matriz de etiquetas asociada
            trainingData.push_back(descriptor.reshape(1, 1));
            classes.push_back(Mat.zeros(new Size(1, 1), CvType.CV_32SC1));
        }
        System.out.println("Leidas " + listFiles.length + " imágenes de entrenamiento -> negativas");
    }    
    
}
